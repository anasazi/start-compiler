{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ControlFlowGraph 
( Vertex(..), Edge(..)
, CFG, buildCFG, linearize
, entry, vertices, blocks, edges
, succs, preds
, reachable
, dominators, idominators, dominanceFrontier
, mapBlocks
) where

import InstructionSet
import BasicBlock 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Array

newtype Vertex = Vertex Integer deriving (Eq, Ord, Enum, Show)
data Edge = Leap Vertex | Fall Vertex deriving (Eq, Ord, Show)
data CFG i = CFG 
  { entry :: Vertex
  , blocks :: M.Map Vertex (BasicBlock i)
  , edges :: M.Map Vertex (S.Set Edge)
  }
  deriving Show
instance Functor CFG where
  fmap f (CFG entry blocks edges) = CFG entry (fmap (fmap f) blocks) edges

mapBlocks f (CFG entry blocks edges) = CFG entry (M.mapWithKey f blocks) edges

vertices = M.keys . blocks

--goesTo (Leap x) = x
--goesTo (Fall x) = x
goesTo = edge id id

edge jump _ (Leap x) = jump x
edge _ fall (Fall x) = fall x

succs :: CFG i -> Vertex -> S.Set Edge
succs cfg v = (edges cfg) M.! v

preds :: CFG i -> Vertex -> S.Set Edge
preds cfg v = S.fromList . swapAround . trimDown . predEntries $ (edges cfg)
  where predEntries = M.filter (S.member v . S.map goesTo)
	trimDown = M.map (S.filter ((==v) . goesTo))
	swapAround = fmap (\(k,v) -> edge (const $ Leap k) (const $ Fall k) (head . S.toList $ v)) . M.toList

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f v | v' == v   = v
	  | otherwise = fixEq f v'
  where v' = f v

dominators :: CFG i -> M.Map Vertex (S.Set Vertex)
dominators cfg = 
  let ivs = zip [0..] $ entry cfg : L.delete (entry cfg) (vertices cfg)
      i2v i = snd . head . filter ((==i) . fst) $ ivs
      v2i v = fst . head . filter ((==v) . snd) $ ivs
      n = fst . last $ ivs
      ps i = S.toList . S.map (v2i . goesTo) . preds cfg . i2v $ i
      dom0 = array (0,n) ((0,[0]) : [(i, [0..n]) | i <- [1..n]])
      domStep dom = array (0,n) ((0,[0]) : [(i, f i) | i <- [1..n]])
	where f n = L.union [n] $ foldr1 L.intersect $ map (dom !) (ps n)
      dom = fixEq domStep dom0
  in M.fromList . map (i2v *** S.fromList . map i2v) $ assocs dom

head' [] = Nothing
head' (x:_) = Just x

idominators :: CFG i -> M.Map Vertex (Maybe Vertex)
idominators cfg =
  let doms = dominators cfg
      idoms = M.mapWithKey (\v ds -> S.delete v ds) $ doms
  in M.map (\ids -> head' . M.keys . M.filter (==ids) $ doms) idoms

dominanceFrontier :: CFG i -> M.Map Vertex (S.Set Vertex)
dominanceFrontier cfg =
  let doms = dominators cfg
      nodes = vertices cfg
      predecessors = M.fromList $ fmap (id &&& preds cfg >>> second (S.map goesTo)) nodes
      strictDom b y = b /= y && b `S.member` (doms M.! y)
      predDoms y = S.unions . fmap (doms M.!) . S.toList $ (predecessors M.! y)
      frontier b = S.fromList $ filter (\y -> not (b `strictDom` y) && b `S.member` predDoms y) nodes
  in M.fromList $ fmap (id &&& frontier) nodes

-- Given the basic blocks for a routine, build the CFG
buildCFG :: (Show i, InstructionSet i) => [BasicBlock i] -> CFG i
buildCFG bs = reachable $ CFG entry blocks edges
  where 
  entry = Vertex 1
  vs = [entry..]
  l2v = M.fromList . concatMap (\(ls, v) -> [ (l,v) | l <- ls ]) $ zip (locs <$> bs) vs
  blocks = M.fromList $ zip vs bs
  edges = M.map (\b -> S.fromList . catMaybes $ [f b, g b]) blocks
    where f b = (Fall . (l2v M.!)) <$> fallsTo b
	  g = fmap (Leap . (l2v M.!)) . jumpsTo

-- remove any unreachable blocks
reachable :: CFG i -> CFG i
reachable cfg = 
  let unmarked = S.delete (entry cfg) $ S.fromList (vertices cfg)
      work = unreachable [entry cfg] unmarked
  in S.foldl removeVertex cfg work
  where unreachable [] unmarked = unmarked
	unreachable (v:vs) unmarked = unreachable vs' unmarked'
	  where newlyMarked = S.map goesTo (succs cfg v) `S.intersection` unmarked
		vs' = S.toList newlyMarked ++ vs
		unmarked' = unmarked S.\\ newlyMarked

removeVertex (CFG entry blocks edges) v
  | v == entry = error "cannot remove the entry vertex"
  | otherwise = CFG entry blocks' edges'
      where blocks' = M.delete v blocks
	    edges' = M.map (S.delete (Leap v) . S.delete (Fall v)) . M.delete v $ edges

-- Given the CFG for a routine, organize the blocks in their linear order
linearize :: InstructionSet i => CFG i -> [BasicBlock i]
linearize cfg = map (blocks cfg M.!) ordering
  where 
    l2v = M.fromList . concatMap (\(v, b) -> [ (l,v) | l <- locs b ]) . M.toList . blocks $ cfg
    fallsToV v = fmap (l2v M.!) . fallsTo $ blocks cfg M.! v
    next = map (id &&& fallsToV) (vertices cfg)
    starts = uncurry (L.\\) . second catMaybes . unzip $ next
    line s = case join $ lookup s next of Nothing -> [s] ; Just s' -> s : line s'
    ordering = concatMap line (entry cfg : filter (/= entry cfg) starts)
