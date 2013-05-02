import Parser
import Pretty
import ControlFlowGraph
import Data.Graph
import IR
import Dominator
import SSA
import qualified Data.Map as M

newline = putStr "\n"

main = do 
    contents <- getContents
    let parseOut = parseProgram contents
    either print doRoutines parseOut

doRoutines ast = mapM_ processRoutine $ routines ast

processRoutine r = do
    let theCFG = cfg r
    let graph = cfgGraph theCFG
    let nlu = cfgNodeLU theCFG
    let vlu = cfgVertexLU theCFG
    let idoms = immediateDominators theCFG
    let (SSA ssacfg) = routineToSSA (NonSSA r)
    -- dump everything
    let key (_,x,_) = x
    let instr = key . nlu
    putStr "routine index: "
    print $ key . nlu $ 0
    let bs = map instr $ vertices graph
    let ssabs = map instr $ vertices $ cfgGraph ssacfg
    mapM_ (\(b,ssa) -> dumpBlock theCFG ssacfg idoms b ssa >> newline) (zip bs ssabs)

dumpBlock cfg ssacfg idoms b ssa = do
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
    putStrLn "  SSA: "
    let (Just ssavert) = cfgVertexLU ssacfg ssa
    let ssaInstructions = (\(x,_,_) -> x) . cfgNodeLU ssacfg $ ssavert
    print . pretty $ ssaInstructions
