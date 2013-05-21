module ControlFlowGraph 
( Vertex
, CFG, buildCFG, linearize
, entry, exit, blocks, succs, preds
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

-- Given the basic blocks for a routine, build the CFG
buildCFG :: InstructionSet i => [BasicBlock i] -> CFG i
buildCFG = hole

-- Given the CFG for a routine, organize the blocks in their linear order
linearize :: InstructionSet i => CFG i -> [BasicBlock i]
linearize = hole
