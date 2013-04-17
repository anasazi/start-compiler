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
    mapM_ (\(r,bb) -> putStrLn "routine"  >> print (pretty r) >> 
		      (mapM_ (\b -> putStrLn "block\t" >> print (pretty b)) bb)) (zip rs bbs)

