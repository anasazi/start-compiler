import Parser
import Pretty
import System.Environment (getArgs)
import ControlFlowGraph
import Data.Tree
import Data.Graph
import IR

newline = putStr "\n"

main = do 
    args <- getArgs
    let filename = head args
    parseOut <- parseProgram filename
    either print doRoutines parseOut
	
doRoutines ast = 
    let rs = routines ast in
    --let bbs = map basicBlocks rs in
    --let ell = map buildEdgeList bbs in
    let cfgs = map cfg rs in
    mapM_ (\cfg -> newline >> doCFG cfg) cfgs

doCFG cfg = do 
    putStrLn . picture . dff $ graph
    putStrLn . picture . flip dfs [0] $ graph
    print . map instr . topSort $ graph
    print . map (\(a,b) -> (instr a, instr b)) . edges $ graph 
    where graph = cfgGraph cfg
	  nodeLU = cfgNodeLU cfg
	  keyLU = cfgKeyLU cfg
	  node (n,_,_) = n
	  label (Instruction n _ _) = n
	  instr = label . head . node . nodeLU
	  picture = drawForest . map (fmap (show . instr))

printEdgeListNode (b,l,ks) = do
    putStrLn "block"
    print $ pretty b
    putStr "label: "
    print l
    putStr "goes to: "
    print ks
    newline

printEdgeList el = mapM_ printEdgeListNode el

printRoutineWithEdgeList (r,el) = do
    putStrLn "routine:"
    print $ pretty r
    printEdgeList el

printRoutineEdgeListCFG (r,el,cfg) = do
    printRoutineWithEdgeList (r,el)
    print $ cfgGraph cfg
