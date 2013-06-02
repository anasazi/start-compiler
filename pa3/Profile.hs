module Profile where

import InstructionSet
import BasicBlock
import Routine
import ControlFlowGraph
import SIF
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Traversable as T
import Pretty
import qualified Data.List as L
import Data.Char
import qualified Data.Map as M
import Data.Maybe

import System.IO
import System.Directory
import System.Cmd

import Debug.Trace

type Counts = M.Map Integer Integer

processCounts :: Counts -> Routines (CFG SIFInstruction) -> Routines ((CFG SIFInstruction), Integer)
processCounts counts cfgs = fmap (analyzeBranches counts) cfgs

analyzeBranches :: Counts -> CFG SIFInstruction -> (CFG SIFInstruction, Integer)
analyzeBranches counts cfg =
  let is = fromBlocks $ linearize cfg
      ves = concat [ [ (v, e) | e <- S.toList es ] | (v, es) <- M.toList (edges cfg) ]
      ve2c = M.fromList $ zip (S.toList $ antiSpanningTree cfg) [0..]
      ve2s0 = M.fromList $ [ (ve, fmap (counts M.!) (M.lookup ve ve2c)) | ve <- ves ]
      ve2s = recoverCounts ve2s0
      -- forward branches are predicted as not taken
      isForward :: Integer -> Integer -> Bool
      isForward src tar = length (takeWhile ((/=src) . loc) is) < length (takeWhile ((/=tar) . loc) is)
      -- backward branches are predicted as taken
      withBranches = M.filter (isBranch . end) (blocks cfg)
      fallFor v = (\((_,a),b) -> (a,b)) . head . M.toList $ M.filterWithKey (\(k,e) _ -> k == v && case e of Fall _ -> True ; _ -> False) ve2s
      leapFor v = (\((_,a),b) -> (a,b)) . head . M.toList $ M.filterWithKey (\(k,e) _ -> k == v && case e of Leap _ -> True ; _ -> False) ve2s
      better = M.mapWithKey (\v b -> betterBranch cfg (fallFor v) (leapFor v) (isForward (loc (end b)) (fromJust $ target (end b))) (end b)) withBranches
      update cfg (v,(br,fall,jump)) = 
	let [a,b] = S.toList $ succs cfg v
	in modifyNode (modifyBlock (\is -> init is ++ [br])) v . addEdgeCFG v fall . addEdgeCFG v jump . removeEdgeCFG v a . removeEdgeCFG v b $ cfg
      numChanged = fromIntegral $ length [ b | b <- map end $ M.elems withBranches, b `notElem` (map (\(x,_,_) -> x) (M.elems better)) ]
  in (foldl update cfg (M.toList better), numChanged)

betterBranch cfg (Fall f,fc) (Leap j,jc) True br@(SIFInstruction loc (Branch (IfZero test) tar)) 
  | fc > jc = (br, Fall f, Leap j) 
  | otherwise = (SIFInstruction loc (Branch (IfSet test) (Label (label $ blocks cfg M.! f))), Fall j, Leap f)
betterBranch cfg (Fall f,fc) (Leap j,jc) False br@(SIFInstruction loc (Branch (IfZero test) tar)) 
  | jc > fc = (br, Fall f, Leap j) 
  | otherwise = (SIFInstruction loc (Branch (IfSet test) (Label (label $ blocks cfg M.! f))), Fall j, Leap f)
betterBranch cfg (Fall f,fc) (Leap j,jc) True br@(SIFInstruction loc (Branch (IfSet test) tar)) 
  | fc > jc = (br, Fall f, Leap j) 
  | otherwise = (SIFInstruction loc (Branch (IfZero test) (Label (label $ blocks cfg M.! f))), Fall j, Leap f)
betterBranch cfg (Fall f,fc) (Leap j,jc) False br@(SIFInstruction loc (Branch (IfSet test) tar)) 
  | jc > fc = (br, Fall f, Leap j) 
  | otherwise = (SIFInstruction loc (Branch (IfZero test) (Label (label $ blocks cfg M.! f))), Fall j, Leap f)

recoverCounts ve2s =
  let nodes = S.toList . S.fromList . map fst . M.keys $ ve2s
      verticesWithOneUnknown = [ v | v <- nodes
				   , let into = filter ((==v) . fst . fst) . M.toList $ ve2s
				   , let outof = filter ((==v) . goesTo . snd . fst) . M.toList $ ve2s 
				   , length (filter (isNothing . snd) into) + length (filter (isNothing . snd) outof) == 1
				   ]
      learnedEdges = [ (ve,Just s) | v <- verticesWithOneUnknown
			      , let into = filter ((==v) . fst . fst) . M.toList $ ve2s
			      , let outof = filter ((==v) . goesTo . snd . fst) . M.toList $ ve2s 
			      , let intoSum = sum . catMaybes . map snd $ into
			      , let outofSum = sum . catMaybes . map snd $ outof
			      , let [(ve,_)] = filter (isNothing . snd) (into ++ outof)
			      , let s = if fst ve == v then intoSum - outofSum else outofSum - intoSum
			      ]
      ve2s' = foldl (flip $ uncurry M.insert) ve2s learnedEdges 
  in if null verticesWithOneUnknown then ve2s else ve2s'

profile numCounters startloc ast = do
  (codepath,codehandle) <- openTempFile "." "instrumented"
  (datapath,datahandle) <- openTempFile "." "stats"

  hPrint codehandle (pretty ast)

  hClose codehandle
  hClose datahandle

  let cmd = "dart " ++ startloc ++ " -r --stats " ++ codepath ++ "> " ++ datapath
  system cmd

  stats <- readFile datapath

  removeFile codepath
  removeFile datapath

  let countStrs = drop 1 . dropWhile (not . L.isPrefixOf "- Counts :") $ lines stats
  let countsStrPairs = map (\s -> (\[a,b] -> "(" ++ a ++ "," ++ b ++ ")") . map (filter isDigit) . words $ s) countStrs
  let readCounts = M.fromList $ map (\s -> read s :: (Integer, Integer)) countsStrPairs
  let counts = M.fromList [ (c, M.findWithDefault 0 c readCounts) | c <- [0..numCounters-1] ]

  return counts
  

instrumentAll :: Routines (CFG SIFInstruction) -> (Routines (CFG SIFInstruction), Integer)
instrumentAll rs =
  let nextInstr = 1 + maxLocCFG rs
      instrumented = evalState (T.mapM instrument rs) nextInstr
      numCounters = sum . map snd . M.elems $ instrumented
  in (M.map fst instrumented, numCounters)

instrument :: CFG SIFInstruction -> State SIFLocation ((CFG SIFInstruction), Integer)
instrument cfg = do
  let toMark = S.toList $ antiSpanningTree cfg
  loc <- get
  let (marked, (loc', _)) = runState (foldM insertCounter cfg toMark) (loc, 0)
  put loc'
  return (marked, fromIntegral $ length toMark)

insertCounter cfg edge = do
  blk <- mkCounter
  return $ splitEdge cfg edge blk

mkCounter :: State (SIFLocation, Integer) [SIFInstruction]
mkCounter = do
  (loc, cnt) <- get
  put (loc + 2, cnt + 1)
  return [SIFInstruction loc (SideEffect (Count (Constant cnt))), SIFInstruction (loc+1) NOP]
