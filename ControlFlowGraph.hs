{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ControlFlowGraph 
{- ( Vertex
, CFG, buildCFG, linearize
, entry, vertices, blocks, edges
, succs, preds
, reachable
) -} where

import InstructionSet
import BasicBlock 
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Array.IArray

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
  l2v = M.fromList $ zip (fmap label bs) vs
  blocks = M.fromList $ zip vs bs
  edges = M.mapWithKey (\v b -> S.fromList . catMaybes $ [f b v, g b]) blocks
    where f b = if falls b then Just . succ else const Nothing
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
		unmarked' = unmarked `S.difference` newlyMarked

removeVertex (CFG entry blocks edges) v
  | v == entry = error "cannot remove the entry vertex"
  | otherwise = CFG entry blocks' edges'
      where blocks' = M.delete v blocks
	    edges' = M.map (S.delete v) . M.delete v $ edges

-- Given the CFG for a routine, organize the blocks in their linear order
linearize :: InstructionSet i => CFG i -> [BasicBlock i]
linearize cfg = fmap (blocks cfg M.!) ordering
  where 
    ordering = let x = entry cfg in x : follows x
    follows v = 
      let b = blocks cfg M.! v
      in if falls b 
	 then case S.toList $ succs cfg v of 
	    [x] -> x : follows x
	    -- TODO does this check work? No. It's comparing a vertex id to a code location.
	    [x,y] -> if Vertex (fromJust (jumpsTo (blocks cfg M.! v))) == x then y : follows y else x : follows x
	  -- TODO can't assume vertices are numbered contiguously
	  else let x = succ v in if x `elem` vertices cfg then x : follows x else []
