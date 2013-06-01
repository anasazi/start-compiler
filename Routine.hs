{-# LANGUAGE NoMonomorphismRestriction #-}
module Routine
( Routines
, routines
) where

import InstructionSet
import BasicBlock 
import SIF 
import Data.Map
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Ord (comparing)

type Routines i = Map SIFMethodDecl i

mloc (SIFMethodDecl _ loc _) = loc

routines :: SIFProgram -> Routines [BasicBlock SIFInstruction]
routines (SIFProgram _ ms _ is) = fromList [ (m, b) | m <- ms, b <- grouper bs, mloc m == loc (leader (head b)) ]
  where bs = toBlocks is :: [BasicBlock SIFInstruction]
	ls = fmap mloc ms :: [SIFLocation]
	grouper bs = case bs of 
	  [] -> []
	  (b:bs) | isEntry b -> let (xs,ys) = break isEntry bs in (b : xs) : grouper ys
		 | otherwise -> grouper $ dropWhile (not . isEntry) bs
		    where isEntry b = loc (leader b) `elem` ls

maxLoc :: InstructionSet i => Routines [BasicBlock i] -> SIFLocation
maxLoc = undefined
--maxLoc rs = loc $ F.maximumBy (comparing loc) rs
