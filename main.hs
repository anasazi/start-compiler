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
  forM_ (toList ssas) $ \(m, ssa) -> do
    --print "as SSA structure:"
    --mapM_ (\(v,b) -> print v >> print (pretty . Vertical . fromBlock $ b)) $ toList . blocks $ ssa
  -- TODO after translating back from SSA, linearize and renumber sequentially.
    print "from SSA:"
    let (m', sif) = evalState (fromSSA m ssa) (-1000)
    pp m'
    mapM_ (\(v,b) -> print v >> print (pretty . Vertical . fromBlock $ b)) $ toList . blocks $ sif
