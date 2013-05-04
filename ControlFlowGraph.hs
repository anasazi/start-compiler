{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ControlFlowGraph 
( Vertex
, CFG, buildCFG, linearize
, entry, vertices, blocks, edges
, succs, preds
, reachable
) where

import InstructionSet
import BasicBlock 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Control.Arrow
import Control.Monad

newtype Vertex = Vertex Integer deriving (Eq, Ord, Enum, Show)
data Edge = Jump Vertex | Fall Vertex deriving (Eq, Show)
data CFG i = CFG 
  { entry :: Vertex
  , blocks :: M.Map Vertex (BasicBlock i)
  , edges :: M.Map Vertex (S.Set Vertex)
  }
  deriving Show

length' = fromIntegral . length

vertices = M.keys . blocks

succs :: CFG i -> Vertex -> S.Set Vertex
succs cfg v = (edges cfg) M.! v

preds :: CFG i -> Vertex -> S.Set Vertex
preds cfg v = S.fromList . fmap fst . onlyMySuccs $ allSuccs
  where allSuccs = zip (vertices cfg) $ fmap (succs cfg) (vertices cfg)
	onlyMySuccs = filter ((S.member v) . snd)

-- Given the basic blocks for a routine, build the CFG
buildCFG :: (Show i, InstructionSet i) => [BasicBlock i] -> CFG i
buildCFG bs = reachable $ CFG entry blocks edges
  where 
  entry = Vertex 1
  vs = [entry..]
  l2v = M.fromList $ concatMap (\(ls, v) -> [ (l,v) | l <- ls ]) $ zip (fmap locs bs) vs
  blocks = M.fromList $ zip vs bs
  edges = M.map (\b -> S.fromList . catMaybes $ [f b, g b]) blocks
    where f b = fmap (l2v M.!) (fallsTo b)
	  g = fmap (l2v M.!) . jumpsTo

-- remove any unreachable blocks
reachable :: CFG i -> CFG i
reachable cfg = 
  let unmarked = S.delete (entry cfg) $ S.fromList (vertices cfg)
      work = unreachable [entry cfg] unmarked
  in S.foldl removeVertex cfg work
  where unreachable [] unmarked = unmarked
	unreachable (v:vs) unmarked = unreachable vs' unmarked'
	  where newlyMarked = succs cfg v `S.intersection` unmarked
		vs' = S.toList newlyMarked ++ vs
		unmarked' = unmarked S.\\ newlyMarked

removeVertex (CFG entry blocks edges) v
  | v == entry = error "cannot remove the entry vertex"
  | otherwise = CFG entry blocks' edges'
      where blocks' = M.delete v blocks
	    edges' = M.map (S.delete v) . M.delete v $ edges

-- Given the CFG for a routine, organize the blocks in their linear order
linearize :: InstructionSet i => CFG i -> [BasicBlock i]
linearize cfg = map (blocks cfg M.!) ordering
  where 
    l2v = M.fromList . concatMap (\(v, b) -> [ (l,v) | l <- locs b ]) . M.toList . blocks $ cfg
    fallsToV v = fmap (l2v M.!) $ fallsTo $ blocks cfg M.! v
    next = map (id &&& fallsToV) (vertices cfg)
    starts = uncurry (L.\\) . second catMaybes . unzip $ next
    line s = case join $ lookup s next of Nothing -> [s] ; Just s' -> s : line s'
    ordering = concatMap line (entry cfg : filter (/= entry cfg) starts)
