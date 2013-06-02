{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module ControlFlowGraph 
( Vertex(..), Edge(..)
, CFG, buildCFG, linearize
, entry, vertices, blocks, edges
, succs, preds
, reachable
, dominators, idominators, dominanceFrontier
, mapBlocks, modifyNode
, goesTo
, removeEmptyVertices
, edge
, spanningTree, reachableNodes
, splitEdge, antiSpanningTree
, addEdgeCFG, removeEdgeCFG
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
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import SIF
import Debug.Trace

newtype Vertex = Vertex Integer deriving (Eq, Ord, Enum, Show)
data Edge = Leap Vertex | Fall Vertex deriving (Eq, Ord, Show)
data CFG i = CFG 
  { entry :: Vertex
  , blocks :: M.Map Vertex (BasicBlock i)
  , edges :: M.Map Vertex (S.Set Edge)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

mapBlocks f (CFG entry blocks edges) = CFG entry (M.mapWithKey f blocks) edges

modifyNode f v cfg = mapBlocks (\v' b -> if v' == v then f b else b) cfg

vertices = M.keys . blocks

goesTo = edge id id

edge jump _ (Leap x) = jump x
edge _ fall (Fall x) = fall x

addEdgeCFG v e (CFG entry blocks edges) = CFG entry blocks $ addEdge v e edges
removeEdgeCFG v e (CFG entry blocks edges) = CFG entry blocks $ removeEdge v e edges

addEdge v e edges = M.adjust (S.insert e) v edges
removeEdge v e edges = M.adjust (S.delete e) v edges

-- the provided block should end in a NOP that can be replaced by the branch if necessary
splitEdge :: CFG SIFInstruction -> (Vertex, Edge) -> [SIFInstruction] -> CFG SIFInstruction
splitEdge cfg@(CFG entry blocks edges) (src, Leap tar) new =
  let v = succ . maximum $ vertices cfg
      from = Leap v
      to = Leap tar
      old@(SIFInstruction loc (Branch op targetLeader)) = end $ blocks M.! src
      SIFInstruction nl _ = head new
      newLeader = Label nl
      fixParent = modifyNode (fmap (\i -> if i == old then SIFInstruction loc (Branch op newLeader) else i)) src
      i' = let SIFInstruction l NOP = last new in SIFInstruction l (Branch Jump targetLeader)
      new' = init new ++ [i']
      b = BB new'
  in fixParent $ CFG entry (M.insert v b blocks) (addEdge src from . M.insert v (S.singleton to) . removeEdge src (Leap tar) $ edges)
splitEdge cfg@(CFG entry blocks edges) (src, Fall tar) new =
  let v = succ . maximum $ vertices cfg
      from = Fall v
      to = Fall tar
      b = BB new
  in CFG entry (M.insert v b blocks) (addEdge src from . M.insert v (S.singleton to) . removeEdge src (Fall tar) $ edges)

succs :: CFG i -> Vertex -> S.Set Edge
succs cfg v = edges cfg M.! v

preds :: CFG i -> Vertex -> S.Set Edge
preds cfg v = S.fromList . swapAround . trimDown . predEntries $ edges cfg
  where predEntries = M.filter (S.member v . S.map goesTo)
	trimDown = M.map (S.filter ((==v) . goesTo))
	swapAround = fmap (\(k,v) -> edge (const $ Leap k) (const $ Fall k) (head . S.toList $ v)) . M.toList

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f v | v' == v   = v
	  | otherwise = fixEq f v'
  where v' = f v

-- v -> things v is dominated by
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

-- v -> immediate dominator of v
idominators :: CFG i -> M.Map Vertex (Maybe Vertex)
idominators cfg =
  let doms = dominators cfg
      idoms = M.mapWithKey S.delete doms
  in M.map (\ids -> head' . M.keys . M.filter (==ids) $ doms) idoms

-- v -> nodes where v's dominance ends
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
  blocksWithExits = M.fromList $ zip vs (zip bs $ exits bs)
  blocks = M.map fst blocksWithExits
  edges = M.map (\(b,x) -> S.fromList . catMaybes $ [f x, g x]) blocksWithExits
    where f x = (Fall . (l2v M.!)) <$> fallsTo x
	  g x = (Leap . (l2v M.!)) <$> jumpsTo x

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

antiSpanningTree cfg =
  let inside = spanningTree cfg
      all = S.fromList $ concat [ [ (v, e) | e <- S.toList es ] | (v, es) <- M.toList $ edges cfg ]
      outside = all S.\\ inside
  in outside

spanningTree :: CFG i -> S.Set (Vertex, Edge)
spanningTree cfg@(CFG entry blocks succs) = f S.empty
  where
  nodes = S.fromList $ vertices cfg
  edges = concat [ [ (v, e) | e <- S.toList es ] | (v, es) <- M.toList succs ]
  g [] = M.fromList [ (v, S.empty) | v <- S.toList nodes ]
  g ((v,e):ves) = addEdge v e $ g ves
  --g ves = M.fromList . map (second S.singleton) . S.toList $ ves
  f es = 
    let found = reachableNodes (CFG entry blocks (g $ S.toList es)) 
	(e:_) = [ x | x <- edges, fst x `S.member` found, goesTo (snd x) `S.notMember` found ]
    in 
    if found == nodes then es else f (S.insert e es)

reachableNodes :: CFG i -> S.Set Vertex
reachableNodes cfg = reachable
  where
  reachable :: S.Set Vertex
  reachable = S.fromList (vertices cfg) S.\\ unreachable [entry cfg] unmarked
  unmarked :: S.Set Vertex
  unmarked = S.delete (entry cfg) $ S.fromList (vertices cfg)
  unreachable :: [Vertex] -> S.Set Vertex -> S.Set Vertex
  unreachable [] us = us
  unreachable (v:vs) us = unreachable vs' us'
    where
    new = S.map goesTo (succs cfg v) `S.intersection` us
    vs' = S.toList new ++ vs
    us' = us S.\\ new

removeVertex (CFG entry blocks edges) v
  | v == entry = error "cannot remove the entry vertex"
  | otherwise = CFG entry blocks' edges'
      where blocks' = M.delete v blocks
	    edges' = M.map (S.delete (Leap v) . S.delete (Fall v)) . M.delete v $ edges

-- removes any blocks that are completely empty and glues together edges to compensate
removeEmptyVertices cfg@(CFG entry blocks edges) =
  let empty = M.keys $ M.filter isEmpty blocks
      rewire e = 
	let v = goesTo . head . S.toList $ succs cfg (goesTo e) in edge (const (Leap v)) (const (Fall v)) e
      edges' = M.map (S.map (\e -> if goesTo e `elem` empty then rewire e else e)) edges
  in foldl removeVertex (CFG entry blocks edges') empty

-- Given the CFG for a routine, organize the blocks in their linear order
linearize :: CFG i -> [BasicBlock i]
linearize cfg = map (blocks cfg M.!) ordering
  where
  falls = M.toList $ M.map (fmap goesTo . listToMaybe . S.toList . S.filter isFall) $ edges cfg
  starts = uncurry (L.\\) . second catMaybes . unzip $ falls
  line s = case join $ lookup s falls of Nothing -> [s] ; Just s' -> s : line s'
  ordering = concatMap line $ entry cfg : filter (/= entry cfg) starts

isFall = edge (const False) (const True)
