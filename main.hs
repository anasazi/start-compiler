import Parser
import Pretty
import System.Environment (getArgs)
import ControlFlowGraph

newline = putStr "\n"

main = do 
    args <- getArgs
    let filename = head args
    parseOut <- parseProgram filename
    case parseOut of 
	Left err -> print err
	Right ast -> doRoutines ast
	
doRoutines ast = 
    let rs = routines ast in
    let bbs = map basicBlocks rs in
    let ell = map buildEdgeList bbs in
    let cfgs = map cfg rs in
    mapM_ printRoutineEdgeListCFG $ zip3 rs ell cfgs

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
