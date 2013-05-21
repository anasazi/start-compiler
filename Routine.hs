module Routine
( Routines
, routines
) where

import InstructionSet
import BasicBlock 
import SIF 
import Data.Map

data Hole = Hole
hole = undefined

type Routines i = Map SIFMethodDecl i

routines :: SIFProgram -> Routines [BasicBlock SIFInstruction]
routines = hole
