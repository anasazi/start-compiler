import Parser
import Pretty
import System.Environment (getArgs)

main = do
    args <- getArgs
    let filename = head args
    parseOut <- parseProgram filename
    case parseOut of
	Left err -> print err
	Right ast -> print . pretty $ ast
