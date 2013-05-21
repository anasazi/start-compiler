module StaticSingleAssignment 
( toSSA, fromSSA
, SSAInstruction
) where

import SIF
import ControlFlowGraph
import InstructionSet

data Hole = Hole
hole = undefined

toSSA :: CFG SIFInstruction -> CFG SSAInstruction
toSSA = hole

fromSSA :: CFG SSAInstruction -> CFG SIFInstruction
fromSSA = hole

-- The SSA data structures
data SSAInstruction = SSA -- TODO
