module InstructionSet where

import Control.Arrow

class InstructionSet a where
  -- what is the number to refer to this instruction as?
  loc :: a -> Integer
  -- is this instruction a control flow jump?
  isJump :: a -> Bool
  -- is conditional branch?
  isBranch :: a -> Bool
  isBranch = isJump &&& canFall >>> uncurry (&&)
  -- the target location of a jump instruction
  target :: a -> Maybe Integer
  -- can this instruction not make a control flow jump?
  canFall :: a -> Bool
  -- is this instruction a method call?
  isCall :: a -> Bool
  -- the target location of a methoc call
  callTarget :: a -> Maybe Integer
  -- is this the main method entry point?
  isMain :: a -> Bool
  -- is this a return?
  isRet :: a -> Bool
  -- how do I do nothing?
  nop :: Integer -> a
