import Parser
import Pretty
import System.Environment (getArgs)
import ControlFlowGraph

main = do
    args <- getArgs
    let filename = head args
    parseOut <- parseProgram filename
    case parseOut of
	Left err -> print err
	--Right ast -> print . pretty $ ast
	Right ast -> mapM_ (putStrLn . ('\n':) . show . pretty . (\(Routine is) -> is)) $ routines ast
