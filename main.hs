import SIF
import Parser
import Pretty
import Routine
import ControlFlowGraph
import BasicBlock
import qualified Data.Map as M
import Control.Monad
import Control.Arrow
import StaticSingleAssignment
import Control.Monad.State
import InstructionSet
import ConstantPropagation
import ValueNumbering
import Profile
import System.Environment (getArgs)

pp :: Pretty a => a -> IO ()
pp = print . pretty

data Hole = Hole
hole = undefined

main = do
  [startloc] <- getArgs
  input <- getContents
  let parsed = readProgram input
  either print (printCFGs startloc) parsed

printCFGs startloc ast@(SIFProgram ts ms gs is)  = do
  let rs = routines ast
  let cfgs = fmap buildCFG rs
  --let instrumented = instrumentAll cfgs
  --let rs' = M.map (fromBlocks . linearize) instrumented
  let ssas = allToSSA cfgs
  let vnums = fmap valueNumbering ssas
  {-
  forM_ ms $ \m -> do
    let ssa = ssas M.! m
    let vnum = vnums M.! m
    forM_ (vertices ssa) $ \v -> do
      print v
      print "SSA"
      pp . Vertical . fromBlock $ blocks ssa M.! v
      print "VNUM"
      pp . Vertical . fromBlock $ blocks vnum M.! v
  -}
  let cprop = fmap constantPropagation vnums
  let sifs = allFromSSA cprop
  let rs' = M.map (fromBlocks . linearize) sifs
  let (ms', is') = uncurry renumber . second concat . unzip . M.toList $ rs'
  let ast' = SIFProgram ts ms' gs is'
  pp ast'
  {-
  let (ms', is') = uncurry renumber . second concat . unzip . M.toList $ rs'
  let ast' = SIFProgram ts ms' gs is'
  counts <- profile startloc ast'
  let profiled = processCounts counts cfgs
  let rs'' = M.map (fromBlocks . linearize) profiled
  let (ms'', is'') = uncurry renumber . second concat . unzip . M.toList $ rs''
  let ast'' = SIFProgram ts ms'' gs is''
  pp ast''
  -}

renumber ms is =
  let old2new = M.fromList $ zip (map loc is) [1..]
      subOperand (Register old) = Register $ old2new M.! old
      subOperand (Label old) = Label $ old2new M.! old
      subOperand oper = oper
      subInstruction (SIFInstruction old opc) = SIFInstruction (old2new M.! old) (fmap subOperand opc)
      is' = fmap subInstruction is
      subMethod (SIFMethodDecl name old vars) = SIFMethodDecl name (old2new M.! old) vars
      ms' = fmap subMethod ms
  in (ms', is')
