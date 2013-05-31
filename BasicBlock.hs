{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module BasicBlock 
( BasicBlock, empty
, leader, end, body
, fallsTo, jumpsTo, label, locs
, toBlocks, fromBlocks, fromBlock
, validateBlock, modifyBlock
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

data BasicBlock i = BB [i] BasicBlockExit deriving (Eq, Show, Functor, Foldable, Traversable)

modifyBlock f (BB is ex) = BB (f is) ex

validateBlock b@(BB is ex) = [b] == toBlocks is

leader (BB is _) = head is
end (BB is _) = last is
exit (BB _ ex) = ex
body (BB is _) = is

-- can this block fall off the end?
fallsTo (BB _ (Fall x)) = Just x
fallsTo (BB _ (Branch _ x)) = Just x
fallsTo _ = Nothing
-- where does this block jump to?
jumpsTo (BB _ (Jump x)) = Just x
jumpsTo (BB _ (Branch x _)) = Just x
jumpsTo _ = Nothing
-- jump target label of this block
label :: InstructionSet i => BasicBlock i -> Integer
label = loc . leader
locs (BB is _) = map loc is

empty n m = BB [nop n] (Fall m)

-- Break an instruction stream into basic blocks
toBlocks :: InstructionSet i => [i] -> [BasicBlock i]
toBlocks is = map wrap blockified
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
	f :: InstructionSet i => [i] -> [([i],Maybe Integer)]
	f [] = []
	f (x:xs) = let (ys,zs) = break isLeader xs in (x : ys, g zs) : f zs
	    where
	    g :: InstructionSet i => [i] -> Maybe Integer
	    g [] = Nothing
	    g (z:_) = Just $ loc z
  wrap :: InstructionSet i => ([i], Maybe Integer) -> BasicBlock i
  wrap (bb, next) | isRet     $ last bb = BB bb Return
		  | isBranch  $ last bb = BB bb $ Branch (fromJust . target . last $ bb) (fromJust next)
		  | isJump    $ last bb = BB bb $ Jump (fromJust . target . last $ bb)
		  | canFall   $ last bb = BB bb $ maybe Cliff Fall next 
		  | otherwise = error "impossible block ending"

-- Unwrap and concatenate the basic blocks
fromBlock (BB is _) = is

fromBlocks :: [BasicBlock i] -> [i]
fromBlocks = concatMap fromBlock

