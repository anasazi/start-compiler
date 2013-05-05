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

pp :: Pretty a => a -> IO ()
pp = print . pretty

main = do
  input <- getContents
  let parsed = readProgram input
  either print printCFGs parsed

printCFGs ast@(SIFProgram ts ms gs is)  = do
  let rs = routines ast
  let cfgs = fmap buildCFG rs
  forM_ (toList cfgs) $ \(m, cfg) -> do
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
    print "as SSA structure:"
    let ssa = toSSA cfg
    mapM_ (\(v,b) -> print v >> print (f b)) $ toList . blocks $ ssa
      where f = pretty . Vertical . fromBlocks . (:[])
    
  {-
  let rs' = fmap (fromBlocks . linearize) cfgs
  let (ms', is') = second concat . unzip $ toList rs'
  let ast' = SIFProgram ts ms' gs is'
  print . pretty $ ast'
  -}
