import Parser
import Pretty
import ControlFlowGraph
import Data.Graph
import IR
import Dominator
import SSA
import ConstantPropagation
import qualified Data.Map as M
import qualified Data.List as L
import System.Environment (getArgs)
import Control.Monad

newline = putStr "\n"

data Opt = OptSCP | OptCSE | OptSSA deriving (Eq, Show)
data Backend = BackendCFG | BackendIR | BackendSSA | BackendREPORT deriving (Eq, Show)

toJust (Just x) = x

processArgs :: [String] -> ([Opt], Backend)
processArgs [back] = ([], processBack)
    where processBack = matchBack . toJust . L.stripPrefix "-backend=" $ back
	  matchBack "ssa" = BackendSSA
	  matchBack "ir" = BackendIR
	  matchBack "cfg" = BackendCFG
	  matchBack "report" = error "report is not supported" --BackendREPORT
processArgs [opt,back] = (processOpt, processBack)
    where processOpt = map matchOpt . lines . map comma2newline . toJust . L.stripPrefix "-opt=" $ opt
	  comma2newline c = if c == ',' then '\n' else c
	  matchOpt "ssa" = OptSSA
	  matchOpt "scp" = OptSCP
	  matchOpt "cse" = OptCSE
	  matchOpt "licm" = error "licm is not supported"
	  matchOpt "copy" = error "copy is not supported"
	  processBack = matchBack . toJust . L.stripPrefix "-backend=" $ back
	  matchBack "ssa" = BackendSSA
	  matchBack "ir" = BackendIR
	  matchBack "cfg" = BackendCFG
	  matchBack "report" = error "report is not supported" --BackendREPORT


main = do 
    args <- getArgs
    let (opts, back) = processArgs args
    contents <- getContents
    let parseOut = parseProgram contents
    --either print (\ast -> doMain (ast, opts, back)) parseOut
    case parseOut of
	Left err -> print err
	Right ast -> doAST ast opts back

doAST ast opts back = do
    let (SSA ssa) = p2ssa $ NonSSA ast
    let (SSA cp) = constantPropagation (SSA ssa)
    let (NonSSA unssa) = ssa2p $ SSA ssa
    let (NonSSA uncp) = ssa2p $ SSA cp
    case back of
	BackendCFG -> doRoutinesCFG ast
	BackendSSA | OptSCP `elem` opts -> print . pretty $ cp
		   | otherwise -> print . pretty $ ssa
	BackendIR | OptSCP `elem` opts -> print . pretty $ uncp
		  | OptSSA `elem` opts -> print . pretty $ unssa
		  | otherwise -> print . pretty $ ast
    {-
    contents <- getContents
    let parseOut = parseProgram contents
--    either print doRoutinesCFG parseOut
    --either print doLinearSSA parseOut
    either print doSCC parseOut
    -}

doMain (ast, opts, back) | OptSCP `elem` opts = doSCP (ast, back)
			 | OptSSA `elem` opts = doSSA (ast, back)
			 | otherwise = doBack (ast, back)
    
doSCP (ast, back) = do
    let ssa = p2ssa $ NonSSA ast
    let (SSA cp) = constantPropagation ssa
    doBack (cp, back)

doSSA (ast, back) = do
    let (SSA ssa) = p2ssa $ NonSSA ast
    doBack (ssa, back)

doBack (code, BackendCFG) = doBackCFG code
doBack (code, BackendIR) = doBackIR code
doBack (code, BackendSSA) = doBackSSA code

doBackCFG = doRoutinesCFG
doBackIR code = print . pretty $ code
doBackSSA code = print . pretty $ code

doSCC ast = do
    let (SSA ssa) = p2ssa $ NonSSA ast
    print . pretty $ ssa
    newline
    let (SSA cp) = constantPropagation (SSA ssa)
    print . pretty $ cp
    newline
    let (NonSSA unssa) = ssa2p $ SSA cp
    print . pretty $ unssa

doLinearSSA ast = do
    let (SSA ssa) = p2ssa $ NonSSA ast
--    print . pretty $ ssa
--    newline
    let (NonSSA unssa) = ssa2p $ SSA ssa
    print . pretty $ unssa

doRoutinesCFG ast = mapM_ processRoutine $ routines ast

processRoutine r = do
    let theCFG = cfg r
    let graph = cfgGraph theCFG
    let nlu = cfgNodeLU theCFG
    let vlu = cfgVertexLU theCFG
    let idoms = immediateDominators theCFG
    --let (SSA ssacfg) = routineToSSA (NonSSA r)
    -- dump everything
    let key (_,x,_) = x
    let instr = key . nlu
    putStr "routine index: "
    print $ key . nlu $ 0
    let bs = map instr $ vertices graph
    --let ssabs = map (key . (cfgNodeLU ssacfg)) $ vertices $ cfgGraph ssacfg
    --mapM_ (\(b,ssa) -> dumpBlock theCFG ssacfg idoms b ssa >> newline) (zip bs ssabs)
    mapM_ (\b -> dumpBlock theCFG idoms b >> newline) bs 

--dumpBlock cfg ssacfg idoms b ssa = do
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
    --putStrLn "  SSA: "
    --let (Just ssavert) = cfgVertexLU ssacfg ssa
    --let ssaInstructions = (\(x,_,_) -> x) . cfgNodeLU ssacfg $ ssavert
    --print . pretty $ ssaInstructions
