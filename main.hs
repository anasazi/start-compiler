import SIF
import Parser
import Pretty
import Routine
import ControlFlowGraph
import BasicBlock
import Data.Map
import Control.Monad
import Control.Arrow

main = do
  input <- getContents
  let parsed = readProgram input
  either print printCFGs parsed

printCFGs ast@(SIFProgram ts ms gs is)  = do
  let rs = routines ast
  let cfgs = fmap buildCFG rs
  let rs' = fmap (fromBlocks . linearize) cfgs
  let (ms', is') = second concat . unzip $ toList rs'
  let ast' = SIFProgram ts ms' gs is'
  print . pretty $ ast'
