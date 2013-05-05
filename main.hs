import SIF
import Parser
import Pretty
import Routine
import ControlFlowGraph
import BasicBlock
import Data.Map
import Control.Monad
import Control.Arrow

pp = print . pretty

main = do
  input <- getContents
  let parsed = readProgram input
  either print printCFGs parsed

printCFGs ast@(SIFProgram ts ms gs is)  = do
  let rs = routines ast
  let cfgs = fmap buildCFG rs
  forM_ (toList cfgs) $ \(m, cfg) -> do
    pp m
    print "edges:"
    mapM_ print $ toList $ edges cfg
    print "doms:"
    let doms = dominators cfg
    mapM_ print $ toList doms
    print "idoms:"
    let idoms = idominators cfg
    mapM_ print $ toList idoms
    
  {-
  let rs' = fmap (fromBlocks . linearize) cfgs
  let (ms', is') = second concat . unzip $ toList rs'
  let ast' = SIFProgram ts ms' gs is'
  print . pretty $ ast'
  -}
