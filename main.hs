import SIF
import Parser
import Pretty
import Routine
import ControlFlowGraph
import BasicBlock
import Data.Map
import Control.Monad
import Control.Arrow
import StaticSingleAssignment
import Control.Monad.State

pp :: Pretty a => a -> IO ()
pp = print . pretty

main = do
  input <- getContents
  let parsed = readProgram input
  either print printCFGs parsed

printCFGs ast@(SIFProgram ts ms gs is)  = do
  let rs = routines ast
  let nextInstr = 1 + maxLocBlocks rs
  let cfgs = fmap buildCFG rs
  let ssas = allToSSA cfgs
  --forM_ (toList cfgs) $ \(m, cfg) -> do
  forM_ (toList ssas) $ \(m, ssa) -> do
    {-
    let v2l = fromList $ fmap (id &&& leader . ((blocks cfg) !)) (vertices cfg)
    pp m
    print "vertex -> location:"
    mapM_ print $ toList v2l
    print "edges:"
    mapM_ print $ toList $ edges cfg
    print "doms:"
    let doms = dominators cfg
    mapM_ print $ toList doms
    print "idoms:"
    let idoms = idominators cfg
    mapM_ print $ toList idoms
    print "dominace frontier:"
    let df = dominanceFrontier cfg
    mapM_ print $ toList df
    -}
    print "as SSA structure:"
--    let ssa = evalState (toSSA m cfg) (-100)
    mapM_ (\(v,b) -> print v >> print (f b)) $ toList . blocks $ ssa
      where f = pretty . Vertical . fromBlock

    
  {-
  let rs' = fmap (fromBlocks . linearize) cfgs
  let (ms', is') = second concat . unzip $ toList rs'
  let ast' = SIFProgram ts ms' gs is'
  print . pretty $ ast'
  -}
