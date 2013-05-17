module InstructionSet where

class InstructionSet a where
  -- what is the number to refer to this instruction as?
  idnum :: a -> Integer
  -- is this instruction the entrance of a function?
  isEnter :: a -> Bool
  -- is this instruction a control flow jump?
  isJump :: a -> Bool
  -- the target location of a jump instruction
  target :: a -> Maybe Integer
