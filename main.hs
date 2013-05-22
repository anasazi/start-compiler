import SIF
import Parser
import Pretty
import Routine
import ControlFlowGraph
import BasicBlock
import Data.Map
import Control.Monad

main = do
  input <- getContents
  let parsed = readProgram input
  either print printCFGs parsed
--  either print (print . pretty) parsed

printCFGs ast = do
  let rs = routines ast
  let cfgs = toList $ fmap buildCFG rs
  forM_ cfgs (\(m, cfg) -> 
    do print m
       print $ entry cfg
       print $ vertices cfg
       print $ edges cfg
       print $ blocks cfg
       let linbs = linearize cfg
       print . pretty . Vertical $ fromBlocks linbs
       putStrLn "")
