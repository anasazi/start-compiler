module ControlFlowGraph where

import IR
import Data.Graph

newtype Routine = Routine [Instruction] deriving Show
newtype BasicBlock = BasicBlock [Instruction] deriving Show

routines :: Program -> [Routine]
routines (Program _ ms _ is) = undefined
  where 
  rLoc = \(Method _ x _) -> x
  iLoc = \(Instruction x _ _) -> x
  

basicBlocks :: Routine -> [(BasicBlock, BasicBlock)]
basicBlocks = undefined

controlFlowGraph :: Routine -> Graph
controlFlowGraph = undefined
