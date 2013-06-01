{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
module StaticSingleAssignment 
( toSSA, fromSSA
, allToSSA, allFromSSA
, SSAInstruction(..)
, SSAOpcode(..)
, SSAOperand(..)
, SSAVar(..)
) where

import SIF
import ControlFlowGraph
import InstructionSet
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State
import Pretty
import Text.PrettyPrint.HughesPJ
import BasicBlock
import qualified Data.Traversable as T
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import Data.Foldable (Foldable)
import Control.Arrow hiding ((<+>))
import Data.Maybe (fromJust)
import Routine

allToSSA :: Routines (CFG SIFInstruction) -> Routines (CFG (SSAInstruction Integer))
allToSSA rs =
  let nextInstr = 1 + maxLocCFG rs
      rsWithMethods = M.mapWithKey (\k v -> (k, v)) rs
  in evalState (T.mapM (uncurry toSSA) rsWithMethods) nextInstr

toSSA :: SIFMethodDecl -> CFG SIFInstruction -> State SIFLocation (CFG (SSAInstruction Integer))
toSSA (SIFMethodDecl _ _ paramsAndVars) cfg = do
  let restructured = fmap (instructionSIF2SSA paramsAndVars) cfg
  inserted <- insertPhis restructured
  let params = [ param | param@(SIFVarDecl _ off _) <- paramsAndVars, off > 0 ]
  let renamed = renamePhis params inserted
  return $ fixJumps cfg renamed

fixJumps old new =
  let updatedLeaders = M.fromList [(loc . leader $ blocks old M.! v, loc . leader $ blocks new M.! v) | v <- vertices old ]
      fixedJumps = updateLabels updatedLeaders new
  in fixedJumps

-- inserting phis where needed
insertPhis :: CFG (SSAInstruction (Maybe Integer)) -> State SIFLocation (CFG (SSAInstruction (Maybe Integer)))
insertPhis cfg = do
  let df = dominanceFrontier cfg
  let sv = fmap stackVars $ blocks cfg
  let insertionLocations v = S.unions . fmap (sv M.!) . M.keys . M.filter (S.member v) $ df
  let workToDo = M.fromList [ (v, insertionLocations v) | v <- vertices cfg ]
  toBeInserted <- T.mapM (mapM createPhi . S.toList) workToDo
  let inserted = mapBlocks (\v b -> modifyBlock (insertAfterEnter $ toBeInserted M.! v) b) cfg
  return inserted

stackVars = S.fromList . fmap target . filter isStackAssignment . body
  where isStackAssignment (SSAInstruction _ (Just (Local{})) _) = True
	isStackAssignment _ = False
	target (SSAInstruction _ (Just x) _) = x

insertAfterEnter xs is = let (pre,suf) = span isEnter is in pre ++ xs ++ suf
  where isEnter (SSAInstruction _ _ opc) =
	  case opc of
	    SIF (SideEffect (Enter _)) -> True
	    SIF (SideEffect Entrypc) -> True
	    _ -> False

createPhi var = do
  loc <- get
  modify (+1)
  return $ SSAInstruction loc (Just var) (Phi M.empty)

-- renaming vars wth subscripts
renamePhis :: [SIFVarDecl] -> CFG (SSAInstruction (Maybe Integer)) -> CFG (SSAInstruction Integer)
renamePhis params cfg =
  let paramsAsVars = S.fromList [ (Local ident off typ Nothing) | (SIFVarDecl ident off typ) <- params ] :: S.Set (SSAVar (Maybe Integer))
      usedVars = (S.unions . fmap stackVars . M.elems . blocks $ cfg) S.\\ paramsAsVars
      state = M.fromList $ [ ((ident, off), (1, [0])) | (Local ident off typ Nothing) <- S.toList paramsAsVars ] 
			++ [ ((ident, off), (1, [0])) | (Local ident off typ Nothing) <- S.toList usedVars ]
      renamed = evalState (rename cfg (entry cfg)) state
      certain = makeCertain renamed
  in certain

updateLabels :: M.Map SIFLocation SIFLocation -> CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
updateLabels old2new cfg = fmap updateInstruction cfg
  where updateInstruction (SSAInstruction loc tar opc) = SSAInstruction loc tar $ updateOpcode opc
    
	updateOpcode (SIF sif) = SIF $ fmap updateOperand sif
	updateOpcode (Copy val) = Copy $ updateOperand val
	updateOpcode x = x

	updateOperand (Val (Label o)) = Val . Label $ M.findWithDefault o o old2new--old2new M.! o
	updateOperand x = x

makeCertain = fmap (fmap fromJust)

rename :: CFG (SSAInstruction (Maybe Integer)) -> Vertex -> State RenameState (CFG (SSAInstruction (Maybe Integer)))
rename !cfg v = do
  -- get block v
  -- generate a name for the target of each phi node
  -- replace variables used and generate names for variables assigned to in block
  renamedNodes <- T.forM (blocks cfg M.! v) renameInstruction
  let cfg' = modifyNode (const renamedNodes) v cfg
  -- update the phi nodes of CFG successors
  let children = fmap (id &&& (blocks cfg' M.!)) . S.toList . S.map goesTo $ succs cfg' v
  children' <- T.mapM (\(c,b) -> markPhis v b >>= \b' -> return (c,b')) children
  let cfg'' = F.foldl' (\g (c,b) -> modifyNode (const b) c g) cfg' children'
  -- recurse into children in dominator tree
  let idoms = idominators cfg''
  let dominated = M.keys $ M.filter (maybe False (==v)) idoms
  cfg''' <- F.foldlM rename cfg'' dominated
  -- pop variables defined in block
  F.mapM_ popPhi $ body (blocks cfg''' M.! v)
  return cfg'''

type RenameState = M.Map (SIFIdentifier, SIFOffset) (Integer, [Integer])

popPhi :: SSAInstruction (Maybe Integer) -> State RenameState ()
popPhi (SSAInstruction _ (Just tar) (Phi _)) = popVar tar
popPhi _ = return ()

markPhis :: Vertex -> BasicBlock (SSAInstruction (Maybe Integer)) -> State RenameState (BasicBlock (SSAInstruction (Maybe Integer)))
markPhis p b = T.forM b $ \i@(SSAInstruction loc tar opc) -> do
  case opc of
    Phi inc -> do
      let (Just to) = tar
      from <- replaceVar to
      let inc' = M.insert p from inc
      return $ SSAInstruction loc tar $ Phi inc'
    _ -> return i
    
renameInstruction :: SSAInstruction (Maybe Integer) -> State RenameState (SSAInstruction (Maybe Integer))
renameInstruction (SSAInstruction loc tar opc) = do
  tar' <- renameSet tar
  renameOpcode loc tar' opc

renameOpcode loc tar opc@(Phi _) = return $ SSAInstruction loc tar opc
renameOpcode loc tar (SIF sif) = do
  sif' <- T.mapM renameUse sif
  return $ SSAInstruction loc tar (SIF sif')
renameOpcode loc tar (Copy val) = do
  val' <- renameUse val
  return $ SSAInstruction loc tar (Copy val')

renameUse :: SSAOperand (Maybe Integer) -> State RenameState (SSAOperand (Maybe Integer))
renameUse (Var var@(Local _ _ _ Nothing)) = fmap Var $ replaceVar var 
renameUse x = return x

renameSet :: Maybe (SSAVar (Maybe Integer)) -> State RenameState (Maybe (SSAVar (Maybe Integer)))
renameSet (Just var@(Local{})) = fmap Just $ genName var 
renameSet (Just var) = return $ Just var
renameSet Nothing = return Nothing

genName :: SSAVar (Maybe Integer) -> State RenameState (SSAVar (Maybe Integer))
genName (Local ident off typ Nothing) = do
  let key = (ident, off)
  (i,_) <- fmap (M.! key) get
  modify $ M.adjust ((+1) *** (i:)) key
  return $ Local ident off typ (Just i)

replaceVar :: SSAVar (Maybe Integer) -> State RenameState (SSAVar (Maybe Integer))
replaceVar var@(Local ident off typ _) = do
  let key = (ident, off)
  (_,(i:_)) <- fmap (M.!key) get
  return $ Local ident off typ (Just i)
replaceVar other = return other

popVar :: SSAVar (Maybe Integer) -> State RenameState ()
popVar (Local ident off _ _) = do
  let key = (ident, off)
  modify $ M.adjust (second tail) key

-- moving data types from SIF to SSA
varSIF2SSA _ (Register loc) = Just $ Reg loc
varSIF2SSA vars (Stack var off) = Just $ Local var off (findType vars (var, off)) (Nothing :: Maybe Integer)
varSIF2SSA _ _ = Nothing

findType vars (ident, off) = let [SIFVarDecl _ _ t] = filter (\(SIFVarDecl a b _) -> a == ident && b == off) vars in t

assignsToSIF vars (SIFInstruction _ (SideEffect (Move _ x))) = varSIF2SSA vars x
assignsToSIF _ (SIFInstruction x (Unary{})) = Just $ Reg x
assignsToSIF _ (SIFInstruction x (Binary{})) = Just $ Reg x
assignsToSIF _ _ = Nothing

operandSIF2SSA vars oper = maybe (Val oper) Var (varSIF2SSA vars oper)

opcodeSIF2SSA vars (SideEffect (Move val _)) = Copy $ operandSIF2SSA vars val
opcodeSIF2SSA vars opc = SIF $ fmap (operandSIF2SSA vars) opc

instructionSIF2SSA vars i@(SIFInstruction loc opc) = SSAInstruction loc (assignsToSIF vars i) (opcodeSIF2SSA vars opc)

--allFromSSA :: Routines (CFG (SSAInstruction Integer)) -> Routines (CFG SIFInstruction)
allFromSSA rs =
  let nextInstr = 1 + maxLocCFG rs
      old = M.toList rs
      new = evalState (T.mapM (uncurry fromSSA) old) nextInstr
  in M.fromList new

--fromSSA :: SIFMethodDecl -> CFG (SSAInstruction Integer) -> State SIFLocation (SIFMethodDecl, CFG SIFInstruction)
fromSSA method@(SIFMethodDecl loc name vars) cfg = do
{- general plan for converting from SSA to SIF
  1. convert assignments to stack variables into assignments to registers (and update uses accordingly).
     record the old stack variable for phi assignments.
     (initial parameter values will remain as stack variables)
-}
  let reg2phi = allPhiAssignments cfg
  let (cfg', var2reg) = stackVarsToRegs cfg
  let cfg'' = subUses var2reg cfg'
{-
  2. replace each phi with a copy from the old stack variable in the node AND copies to the old stack variable in the predecessors (last instructions before a jump).
     (the stack variable isn't used anywhere else because of step 1)
-}
  cfg''' <- expandPhis reg2phi cfg''
{-
  3. propagate copies to registers by replacing uses of that register with the copied value and removing the assignment. iterate until fixed point.
     (only copies into the stack variables used for phi nodes will be left)
-}
  let cfg'''' = copyPropagation cfg'''
  --let cfg'''' = cfg'''
{-
  4. replace remaining copies (all to stack variables) with moves. unwrap all other SIF opcodes.
     (there should be no phi opcodes left)
-}
  let nextOffset = minimum (0 : map (\(SIFVarDecl _ x _) -> x) vars) - 4
  let newStackSlots = S.toList $ newStackVars cfg''''
  let slotMapping = M.fromList $ zip newStackSlots [nextOffset, nextOffset - 4 ..]
  let cfg''''' = fmap (instructionSSA2SIF slotMapping) cfg''''
{-
  5. update method declaration with the new local variables (one per phi node).
     (parameters in method declaration are unchanged)
-}
  let oldVarDecls = [ SIFVarDecl (phiVarName ident 0) off typ | (SIFVarDecl ident off typ) <- vars ]
  let newVarDecls = [ SIFVarDecl (phiVarName ident sub) off typ | (Local ident _ typ sub, off) <- M.toList slotMapping ]
  let newMethod = SIFMethodDecl loc name (oldVarDecls ++ newVarDecls)
  return (newMethod, cfg''''')

phiVarName ident sub = ident ++ "$" ++ show sub

varSSA2SIF _ (Reg loc) = Register loc
varSSA2SIF newOffs var@(Local ident off typ sub) = Stack (phiVarName ident sub) $ M.findWithDefault off var newOffs

operandSSA2SIF newOffs (Var var) = varSSA2SIF newOffs var
operandSSA2SIF _ (Val val) = val

opcodeSSA2SIF _ _ (Phi _) = error "there should not be any phi nodes"
opcodeSSA2SIF _ newOffs (SIF sif) = fmap (operandSSA2SIF newOffs) sif
opcodeSSA2SIF (Just (Reg tar)) newOffs (Copy val) = Binary Add (operandSSA2SIF newOffs val) (Constant 0) Dynamic
opcodeSSA2SIF (Just tar) newOffs (Copy val) = SideEffect (Move (operandSSA2SIF newOffs val) (varSSA2SIF newOffs tar))

instructionSSA2SIF newOffs (SSAInstruction loc tar opc) = SIFInstruction loc (opcodeSSA2SIF tar newOffs opc)

newStackVars cfg = S.fromList . fmap target . filter isStackAssignment . fromBlocks . M.elems . blocks $ cfg
  where isStackAssignment (SSAInstruction _ (Just (Local _ _ _ sub)) _) | sub > 0 = True
	isStackAssignment _ = False
	target (SSAInstruction _ (Just x) _) = x

copyPropagation :: CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
copyPropagation cfg = fixEq copyPropagationOnce cfg

copyPropagationOnce cfg =
  let withoutCopies = mapBlocks (\_ b -> modifyBlock (filter needToKeep) b) cfg
      allCopies = filter isCopyToReg . fromBlocks . M.elems $ blocks cfg
      old2new = M.fromList $ fmap (\(SSAInstruction _ (Just reg) (Copy val)) -> (reg, val)) allCopies
      subbed =  fixEq (subUses' old2new) withoutCopies -- TODO do dead code elimination separately
  in fixJumps cfg subbed
  where
  isCopyToReg (SSAInstruction _ (Just (Reg _)) (Copy _)) = True
  isCopyToReg _ = False
  needToKeep i = not $ isCopyToReg i 

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f v | v' == v   = v
	  | otherwise = fixEq f v'
  where v' = f v

isPhi (SSAInstruction _ _ (Phi _)) = True
isPhi _ = False

allPhiAssignments :: CFG (SSAInstruction Integer) -> M.Map (SSAVar Integer) (SSAVar Integer)
allPhiAssignments cfg = 
  let phis = filter isPhi . fromBlocks . M.elems $ blocks cfg
  in M.fromList [ (Reg reg, var) | SSAInstruction reg (Just var) _ <- phis ]

expandPhis reg2phi cfg = F.foldlM (expandBlockPhis reg2phi) cfg (vertices cfg)

expandBlockPhis reg2phi cfg vert = do
  let replaced = modifyNode (fmap $ \i -> if isPhi i then replacePhi reg2phi i else i) vert cfg
  work <- T.mapM (genPhiCopies reg2phi) $ filter isPhi $ fromBlock $ blocks cfg M.! vert
  let toBeInserted = M.unionsWith (++) $ fmap (fmap (:[])) work
  let insert v b = modifyBlock (insertBeforeBranch $ M.findWithDefault [] v toBeInserted) b
  let inserted = mapBlocks insert replaced
  return inserted

replacePhi reg2phi (SSAInstruction loc (Just (Reg reg)) (Phi _)) = SSAInstruction loc (Just (Reg reg)) (Copy (Var $ reg2phi M.! (Reg reg)))

genPhiCopies reg2phi (SSAInstruction _ (Just (Reg reg)) (Phi inc)) =
  let tar = reg2phi M.! Reg reg
  in T.forM inc $ createPhiCopy tar

createPhiCopy tar src = do
  loc <- get
  modify (+1)
  return $ SSAInstruction loc (Just tar) (Copy $ Var src)

insertBeforeBranch xs is = let (pre, suf) = break isJump is in pre ++ xs ++ suf

stackVarsToRegs :: CFG (SSAInstruction Integer) -> (CFG (SSAInstruction Integer), M.Map (SSAVar Integer) (SSAVar Integer))
stackVarsToRegs cfg =
  let isStackVarAssignment (SSAInstruction _ (Just (Local{})) _) = True
      isStackVarAssignment _ = False
      assignments = filter isStackVarAssignment . fromBlocks . M.elems $ blocks cfg
      var2reg = M.fromList [ (var, Reg reg) | SSAInstruction reg (Just var) _ <- assignments ]
      f (SSAInstruction reg (Just (Local{})) opc) = SSAInstruction reg (Just (Reg reg)) opc
      f i = i
  in (fmap f cfg, var2reg)

subUses :: M.Map (SSAVar Integer) (SSAVar Integer) -> CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
subUses old2new cfg =
  let sub (SSAInstruction loc tar opc) = SSAInstruction loc tar $ subOpcode old2new opc
  in fmap sub cfg

subUses' :: M.Map (SSAVar Integer) (SSAOperand Integer) -> CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
subUses' old2new cfg =
  let sub (SSAInstruction loc tar opc) = SSAInstruction loc tar $ subOpcode' old2new opc
  in fmap sub cfg

subVar :: M.Map (SSAVar Integer) (SSAVar Integer) -> SSAVar Integer -> SSAVar Integer
subVar old2new var = M.findWithDefault var var old2new

subOperand old2new (Var var) = Var $ subVar old2new var
subOperand _ val = val

subOperand' old2new (Var var) = M.findWithDefault (Var var) var old2new
subOperand' old2new val = val

subOpcode old2new (Phi inc) = Phi $ fmap (subVar old2new) inc
subOpcode old2new (SIF sif) = SIF $ fmap (subOperand old2new) sif
subOpcode old2new (Copy val) = Copy $ subOperand old2new val

subOpcode' old2new (SIF sif) = SIF $ fmap (subOperand' old2new) sif
subOpcode' old2new (Copy val) = Copy $ subOperand' old2new val

-- The SSA data structures
data SSAVar a = 
    Reg SIFLocation -- a register
  | Local SIFIdentifier SIFOffset SIFType a -- a local or parameter
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data SSAOperand a = 
    Var (SSAVar a) -- equivalent of Stack and Register
  | Val SIFOperand -- no Stack or Register uses
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data SSAOpcode a = 
    Phi (M.Map Vertex (SSAVar a))	  
  | SIF (SIFOpcode (SSAOperand a)) -- no uses of Move. either sets a register or has an effect
  | Copy (SSAOperand a) -- equivalent of Move, but also allows for copying into a register
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data SSAInstruction a = SSAInstruction SIFLocation (Maybe (SSAVar a)) (SSAOpcode a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance InstructionSet (SSAInstruction a) where
  loc (SSAInstruction x _ _) = x

  isJump (SSAInstruction _ _ (SIF (Branch _ _))) = True
  isJump _ = False

  target (SSAInstruction _ _ (SIF (Branch _ (Val (Label l))))) = Just l
  target _ = Nothing

  canFall (SSAInstruction _ _ (SIF (Branch Jump _))) = False
  canFall (SSAInstruction _ _ (SIF (SideEffect (Ret _)))) = False
  canFall _ = True

  isCall (SSAInstruction _ _ (SIF (SideEffect (Call _)))) = True
  isCall _ = False

  callTarget (SSAInstruction _ _ (SIF (SideEffect (Call (Val (Label l)))))) = Just l
  callTarget _ = Nothing

  isMain (SSAInstruction _ _ (SIF (SideEffect Entrypc))) = True
  isMain _ = False

  isRet (SSAInstruction _ _ (SIF (SideEffect (Ret _)))) = True
  isRet _ = False

  nop n = SSAInstruction n Nothing (SIF NOP)

instance Show a => Pretty (SSAVar a) where
  pretty (Reg loc) = parens . integer $ loc
  pretty (Local var _ _ a) = text (var ++ "$" ++ show a)

instance Show a => Pretty (SSAOperand a) where
  pretty (Var var) = pretty var
  pretty (Val val) = pretty val

instance Show a => Pretty (SSAOpcode a) where
  pretty (SIF sif) = pretty sif
  pretty (Copy val) = text "copy" <+> pretty val
  pretty (Phi inc) = text "phi" <+> pretty (Horizontal $ M.elems inc)

instance Show a => Pretty (SSAInstruction a) where
  pretty (SSAInstruction loc Nothing opc) = text "    instr" <+> integer loc <> colon <+> pretty opc
  pretty (SSAInstruction loc (Just var) opc) = text "    instr" <+> integer loc <> colon <+> pretty var <+> text "<-" <+> pretty opc
