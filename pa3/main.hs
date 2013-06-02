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
import System.IO
--import System.Console.GetOpt
import qualified Data.List as L
import Data.Maybe
import Text.PrettyPrint.HughesPJ (($$))

pp :: Pretty a => a -> IO ()
pp = print . pretty

data Optimizations = OptSSA | OptSCP | OptCSE | OptCOPY | OptPROF deriving (Eq, Show)
data Backends = BackCFG | BackSIF | BackReport | BackSSA deriving (Eq, Show)

parseOpt x = do
  suf <- L.stripPrefix "-opt=" x
  let y = words $ map (\c -> if c == ',' then ' ' else c) suf
  let opts = catMaybes $ map toOpt y
  return opts

toOpt "ssa" = Just OptSSA
toOpt "scp" = Just OptSCP
toOpt "cse" = Just OptCSE
toOpt "copy" = Just OptCOPY
toOpt "prof" = Just OptPROF
toOpt _ = Nothing

parseBackend x = do
  suf <- L.stripPrefix "-backend=" x
  back <- toBackend suf
  return back

toBackend "cfg" = Just BackCFG
toBackend "ir" = Just BackSIF
toBackend "report" = Just BackReport
toBackend "ssa" = Just BackSSA
toBackend _ = Nothing

parseStartLoc x = do
  suf <- L.stripPrefix "-start=" x
  return suf

main = do
  args <- getArgs
  [opts,backend,startloc] <- getArgs
  let (Just opts') = parseOpt opts
  let (Just backend') = parseBackend backend
  let (Just startloc') = parseStartLoc startloc
  input <- getContents
  let parsed = readProgram input
  either print (work opts' backend' startloc') parsed

work opts backend startloc ast = do
  let rs = routines ast
  let cfgs = fmap buildCFG rs
  profiled <- (if OptPROF `elem` opts then doProfile backend startloc ast else return) cfgs
  if OptSSA `elem` opts
    then do
      let ssas = allToSSA profiled
      vnums <- (if OptCSE `elem` opts then doValueNumbering backend else return) ssas
      cprop <- (if OptSCP `elem` opts then doConstantProp backend else return) vnums 
      case backend of
	BackCFG -> do
	  forM_ (M.toList cprop) $ \(m, ssa) -> do
	    pp m
	    forM_ (M.toList $ blocks ssa) $ \(v, b) -> do
	      print v
	      pp . Vertical . fromBlock $ b
	BackSIF -> do
	  sifs <- (if OptCOPY `elem` opts then doCopyProp backend else return . allFromSSA) cprop
	  ast' <- toASTsif ast sifs
	  pp ast'
	BackReport -> return ()
	BackSSA -> do 
	  (ts,ms,gs,is) <- toASTssa ast cprop
	  print (pretty (Vertical ts) $$ pretty (Vertical ms) $$ pretty (Vertical gs) $$ pretty (Vertical is))
    else case backend of
      BackCFG -> do
	forM_ (M.toList cfgs) $ \(m, cfg) -> do
	  pp m
	  forM_ (M.toList $ blocks cfg) $ \(v, b) -> do
	    print v
	    pp . Vertical . fromBlock $ b
      BackSIF -> toASTsif ast cfgs >>= pp
      BackReport -> return ()
      BackSSA -> error "Cannot emit SSA without doing SSA"

toASTssa ast@(SIFProgram ts _ gs _) ssas = do
  let rs = M.map (fromBlocks . linearize) ssas
  let (ms, is) = uncurry renumberSSA . second concat . unzip . M.toList $ rs
  return (ts, ms, gs, is)

toASTsif ast@(SIFProgram ts _ gs _) sifs = do
  let rs = M.map (fromBlocks . linearize) sifs 
  let (ms, is) = uncurry renumberSIF . second concat . unzip . M.toList $ rs
  let ast' = SIFProgram ts ms gs is
  return ast'

doCopyProp backend ssas = do
  let sifs' = allFromSSACopyProp ssas
  let sifs = fmap fst sifs'
  when (backend == BackReport) 
    (forM_ (M.toList $ fmap snd sifs') $ \(SIFMethodDecl name loc _, n) -> do
      putStrLn $ "#Function: " ++ name ++ "@" ++ show loc
      putStrLn $ "#Number of copies propagated: " ++ show n
    )
  return sifs

doValueNumbering backend ssas = do
  let vnums' = fmap valueNumbering ssas
  let vnums = fmap fst vnums'
  when (backend == BackReport) 
    (forM_ (M.toList $ fmap snd vnums') $ \(SIFMethodDecl name loc _, n) -> do
      putStrLn $ "#Function: " ++ name ++ "@" ++ show loc
      putStrLn $ "#Number of expressions eliminated: " ++ show n
    )
  return vnums

doConstantProp backend ssas = do
  let cprop' = fmap constantPropagation ssas
  let cprop = fmap fst cprop'
  when (backend == BackReport) 
    (forM_ (M.toList $ fmap snd cprop') $ \(SIFMethodDecl name loc _, n) -> do
      putStrLn $ "#Function: " ++ name ++ "@" ++ show loc
      putStrLn $ "#Number of constants propagated: " ++ show n
    )
  return cprop

doProfile backend startloc ast@(SIFProgram ts _ gs _) sifs = do
  let (instrumented, numCounters) = instrumentAll sifs
  ast' <- toASTsif ast instrumented
  counts <- profile numCounters startloc ast'
  let profiled' = processCounts counts sifs
  let profiled = fmap fst profiled'
  when (backend == BackReport) 
    (forM_ (M.toList $ fmap snd profiled') $ \(SIFMethodDecl name loc _, n) -> do
      putStrLn $ "#Function: " ++ name ++ "@" ++ show loc
      putStrLn $ "#Number of branches predicted: " ++ show n
    )
  return profiled

renumberSIF ms is =
  let old2new = M.fromList $ zip (map loc is) [1..]
      subOperand (Register old) = Register $ old2new M.! old
      subOperand (Label old) = Label $ old2new M.! old
      subOperand oper = oper
      subInstruction (SIFInstruction old opc) = SIFInstruction (old2new M.! old) (fmap subOperand opc)
      is' = fmap subInstruction is
      subMethod (SIFMethodDecl name old vars) = SIFMethodDecl name (old2new M.! old) vars
      ms' = fmap subMethod ms
  in (ms', is')

renumberSSA ms is =
  let old2new = M.fromList $ zip (map loc is) [1..]
      subVar (Reg old) = Reg $ old2new M.! old
      subVar var = var
      subOperand (Var var) = Var (subVar var)
      subOperand (Val (Label old)) = Val (Label $ old2new M.! old)
      subOperand oper = oper
      subOpcode (Phi inc) = Phi $ fmap subVar inc
      subOpcode (SIF sif) = SIF $ fmap subOperand sif
      subOpcode (Copy val) = Copy $ subOperand val
      subInstruction (SSAInstruction old tar opc) = SSAInstruction (old2new M.! old) (fmap subVar tar) (subOpcode opc)
      is' = fmap subInstruction is
      subMethod (SIFMethodDecl name old vars) = SIFMethodDecl name (old2new M.! old) vars
      ms' = fmap subMethod ms
  in (ms', is')
