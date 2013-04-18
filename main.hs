{-# LANGUAGE BangPatterns #-}
import Parser
import Pretty
import System.Environment (getArgs)
import ControlFlowGraph
import Data.Tree
import Data.Graph
import IR
import Dominator
import System.Time
import qualified Data.Map as M

newline = putStr "\n"

main = do 
    !args <- getArgs
    let !filename = head args
    !parseOut <- parseProgram filename
    either print doRoutines parseOut
	
doRoutines !ast = mapM_ processRoutine $! routines ast

processRoutine !r = do
    -- make sure we've built everything fully!
    let !theCFG = cfg r
    let !graph = cfgGraph theCFG
    let !nlu = cfgNodeLU theCFG
    let !vlu = cfgVertexLU theCFG
    -- actully time the dominator computation
    !start <- getClockTime
    let !doms = dominators theCFG
    let !idoms = idom doms
    !end <- getClockTime
    let diff = diffClockTimes end start
    print diff
    -- dump everything
    let key (_,x,_) = x
    let instr = key . nlu
    putStr "routine index: "
    print $ key . nlu $ 0
    let bs = map instr $ vertices graph
    mapM_ (\b -> dumpBlock theCFG idoms b >> newline) bs

dumpBlock cfg idoms b = do
    putStr "  block index: "
    print b
    let (Just vert) = cfgVertexLU cfg b
    let instructions = (\(x,_,_) -> x) . cfgNodeLU cfg $ vert
    print . pretty $ instructions
    putStr "  preds: "
    print $ preds cfg b
    putStr "  succs: "
    print $ succs cfg b
    putStr "  idom: "
    putStrLn $ maybe "Nothing" show $ idoms M.! b
