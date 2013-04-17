module ControlFlowGraph where

import IR
import Data.Graph

newtype Routine = Routine [Instruction] deriving Show
newtype BasicBlock = BasicBlock [Instruction] deriving Show
newtype ControlFlowEdge = CFGE (BasicBlock, BasicBlock)
type NodeLookup = Vertex -> (BasicBlock, Integer, [Integer])
type KeyLookup = Integer -> Maybe Vertex
data ControlFlowGraph = CFG { graph :: Graph, nodeLookup :: NodeLookup, keyLookup :: KeyLookup }

--routines :: Program -> [Routine]
routines (Program _ ms _ is) = map (Routine . getInBounds) bounds
  where 
  bounds = zip (map rloc ms) (drop 1 (map rloc ms) ++ [1 + iloc (last is)])
  getInBounds (a,b) = filter (\i -> iloc i >= a && iloc i < b) is
  rloc = \(Method _ x _) -> x
  iloc = \(Instruction x _ _) -> x
  

basicBlocks :: Routine -> [ControlFlowEdge]
basicBlocks = undefined

label :: [ControlFlowEdge] -> [(BasicBlock, Integer, [Integer])]
label = undefined

cfg :: Routine -> ControlFlowGraph
cfg = (\(g,n,k) -> CFG g n k) . graphFromEdges . label . basicBlocks
