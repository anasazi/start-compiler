module BasicBlock 
( BasicBlock
, leader
, end
, blocks
) where

import InstructionSet
import Data.Function (on)
import Data.List (nub, groupBy)
import Data.Maybe (catMaybes)

data Hole = Hole
hole = undefined

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

blocks :: InstructionSet i => [i] -> [BasicBlock i]
blocks is = map BB blockified
  where 
  leaders = nub $ entrypc ++ calls ++ jumps ++ falls
	where 
	entrypc = [ loc i | i <- is, isMain i ]
	calls = [ l | i <- is, isCall i, let Just l = callTarget i ]
	jumps = catMaybes [ jumpTarget i | i <- is, isJump i ]
	falls = [ l | (i1,i2) <- zip is (drop 1 is), isJump i1, let l = loc i2 ]
  blockified = f . dropWhile (not . isLeader) $ is
	where
	isLeader :: InstructionSet i => i -> Bool
	isLeader = (`elem` leaders) . loc
	f :: InstructionSet i => [i] -> [[i]]
	f [] = []
	f (x:xs) = let (ys,zs) = break isLeader xs in (x : ys) : f zs

