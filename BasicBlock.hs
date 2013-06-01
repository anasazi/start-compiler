{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module BasicBlock 
( BasicBlock--, empty
, leader, end, body
, fallsTo, jumpsTo, label, locs
, toBlocks, fromBlocks, fromBlock
, validateBlock
, modifyBlock
, exits
) where

import InstructionSet
import Data.Function (on)
import Data.List (nub, groupBy)
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

{- A non-empty straight-line sequence of instructions without control 
 - flow between them. The first instruction (leader) is 
 - a) the entrence of a method/program or b) a jump target.
 - The last instruction (exit) is a) a branch or b) the instruction 
 - before a jump target.
 -}
data BasicBlockExit = 
    Return
  | Cliff
  | Fall Integer 
  | Jump Integer 
  | Branch Integer Integer 
  deriving (Eq, Show)

data BasicBlock i = BB [i] deriving (Eq, Show, Functor, Foldable, Traversable)

modifyBlock f (BB is) = BB (f is)

validateBlock b@(BB is) = [b] == toBlocks is

leader (BB is) = head is
end (BB is) = last is
body (BB is) = is

-- can this block fall off the end?
fallsTo (Fall x) = Just x
fallsTo (Branch _ x) = Just x
fallsTo _ = Nothing
-- where does this block jump to?
jumpsTo (Jump x) = Just x
jumpsTo (Branch x _) = Just x
jumpsTo _ = Nothing
-- jump target label of this block
label :: InstructionSet i => BasicBlock i -> Integer
label = loc . leader
locs (BB is) = map loc is

empty n = BB [nop n]

-- Break an instruction stream into basic blocks
toBlocks :: InstructionSet i => [i] -> [BasicBlock i]
toBlocks is = map BB blockified
  where 
  leaders = nub $ entrypc ++ calls ++ jumps ++ falls
	where 
	entrypc = [ loc i | i <- is, isMain i ]
	calls = [ l | i <- is, isCall i, let Just l = callTarget i ]
	jumps = catMaybes [ target i | i <- is, isJump i ]
	falls = [ l | (i1,i2) <- zip is (drop 1 is), isJump i1 || isRet i1, let l = loc i2 ]
  blockified = f . dropWhile (not . isLeader) $ is
	where
	isLeader :: InstructionSet i => i -> Bool
	isLeader = (`elem` leaders) . loc
	f :: InstructionSet i => [i] -> [[i]]
	f [] = []
	f (x:xs) = let (ys,zs) = break isLeader xs in (x : ys) : f zs

wrap :: InstructionSet i => BasicBlock i -> Maybe Integer -> BasicBlockExit
wrap  bb next	| isRet     $ end bb = Return
		| isBranch  $ end bb = Branch (fromJust . target . end $ bb) (fromJust next)
		| isJump    $ end bb = Jump (fromJust . target . end $ bb)
		| canFall   $ end bb = maybe Cliff Fall next 
		| otherwise = error "impossible block ending"

-- assumes that blocks are passed in their proper linear order
exits :: InstructionSet i => [BasicBlock i] -> [BasicBlockExit]
exits [] = []
exits (a:[]) = [wrap a Nothing]
exits (a:b:cs) = wrap a (Just $ label b) : exits (b:cs)

-- Unwrap and concatenate the basic blocks
fromBlock (BB is) = is

fromBlocks :: [BasicBlock i] -> [i]
fromBlocks = concatMap fromBlock

