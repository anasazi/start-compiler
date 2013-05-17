module BasicBlock 
( BasicBlock
, leader
, end
, toBlock
) where

{-
 - A non-empty straight-line sequence of instructions without control flow between them.
 - The first instruction (leader) is a) the entrence of a method/program or b) a jump target.
 - The last instruction (exit) is a) a branch or b) the instruction before a jump target.
 -}
newtype BasicBlock i = BB { runBB :: [i] } 
instance Functor BasicBlock where
  fmap f = BB . fmap f . runBB

leader = head . runBB
end = last . runBB

toBlock = BB
