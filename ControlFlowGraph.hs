module ControlFlowGraph 
( Vertex
, CFG, buildCFG, linearize
, entry, exit, blocks, succs, preds
, mapBlocks
) where

import InstructionSet
import BasicBlock
import Data.Map
import Data.Set

data Hole = Hole
hole = undefined

newtype Vertex = Vertex Integer deriving Eq
data CFG i = CFG 
  { entry :: Vertex
  , exit :: Vertex
  , blocks :: Map Vertex (BasicBlock i)
  , succs :: Map Vertex (Set Vertex)
  , preds :: Map Vertex (Set Vertex) 
  }

mapBlocks :: (BasicBlock a -> BasicBlock b) -> CFG a -> CFG b
mapBlocks f (CFG i o bs ss ps) = CFG i o (fmap f bs) ss ps

buildCFG :: InstructionSet i => [i] -> CFG i
buildCFG = hole

linearize :: CFG a -> [a]
linearize = hole
