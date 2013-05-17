import SIF
import Parser
import Pretty

main = do
  input <- getContents
  let parsed = readProgram input
  either print (print . pretty) parsed
