{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
module StaticSingleAssignment 
( toSSA, fromSSA
, SSAInstruction
, allToSSA
) where

import SIF
import ControlFlowGraph
import InstructionSet
import qualified Data.Map as M
import qualified Data.Set as S
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

data Hole = Hole
hole = undefined

allToSSA :: Routines (CFG SIFInstruction) -> Routines (CFG (SSAInstruction Integer))
allToSSA rs =
  let nextInstr = 1 + maxLocCFG rs
      rsWithMethods = M.mapWithKey (\k v -> (k, v)) rs
  in evalState (T.mapM (uncurry toSSA) rsWithMethods) nextInstr

toSSA :: SIFMethodDecl -> CFG SIFInstruction -> State SIFLocation (CFG (SSAInstruction Integer))
toSSA (SIFMethodDecl _ _ params) cfg = do
  let restructured = fmap instructionSIF2SSA cfg
  inserted <- insertPhis restructured
  let renamed = renamePhis params inserted
  let updatedLeaders = M.fromList [(loc . leader $ blocks cfg M.! v, loc . leader $ blocks renamed M.! v) | v <- vertices cfg ]
  let fixedJumps = updateLabels updatedLeaders renamed
  return fixedJumps

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
  let paramsAsVars = S.fromList [ (Local ident off Nothing) | (SIFVarDecl ident off _) <- params ] :: S.Set (SSAVar (Maybe Integer))
      usedVars = (S.unions . fmap stackVars . M.elems . blocks $ cfg) S.\\ paramsAsVars
      state = M.fromList $ [ ((ident, off), (1, [0])) | (Local ident off Nothing) <- S.toList paramsAsVars ] 
			++ [ ((ident, off), (0, [])) | (Local ident off Nothing) <- S.toList usedVars ]
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
rename cfg v = do
  -- get block v
  -- generate a name for the target of each phi node
  -- replace variables used and generate names for variables assigned to in block
  renamedNodes <- T.forM (blocks cfg M.! v) renameInstruction
  let cfg' = modifyNode (const renamedNodes) v cfg
  -- update the phi nodes of CFG successors
  let children = fmap (id &&& (blocks cfg' M.!)) . S.toList . S.map goesTo $ succs cfg' v
  children' <- T.mapM (\(c,b) ->  markPhis v b >>= \b' -> return (c,b')) children
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

markPhis :: Vertex -> BasicBlock (SSAInstruction (Maybe Integer)) ->
	    State RenameState (BasicBlock (SSAInstruction (Maybe Integer)))
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
  case opc of
    Phi _ -> return $ SSAInstruction loc tar' opc 
    SIF sif -> T.mapM renameUse sif >>= \sif' -> return $ SSAInstruction loc tar' (SIF sif')
    Copy val -> renameUse val >>= \val' -> return $ SSAInstruction loc tar' (Copy val')

renameUse :: SSAOperand (Maybe Integer) -> State RenameState (SSAOperand (Maybe Integer))
renameUse (Var var@(Local _ _ Nothing)) = fmap Var $ replaceVar var 
renameUse x = return x

renameSet :: Maybe (SSAVar (Maybe Integer)) -> State RenameState (Maybe (SSAVar (Maybe Integer)))
renameSet (Just var@(Local{})) = fmap Just $ genName var 
renameSet (Just var) = return $ Just var
renameSet Nothing = return Nothing

genName :: SSAVar (Maybe Integer) -> State RenameState (SSAVar (Maybe Integer))
genName (Local ident off Nothing) = do
  let key = (ident, off)
  (i,_) <- fmap (M.! key) get
  modify $ M.adjust ((+1) *** (i:)) key
  return $ Local ident off (Just i)

replaceVar :: SSAVar (Maybe Integer) -> State RenameState (SSAVar (Maybe Integer))
replaceVar (Local ident off Nothing) = do
  let key = (ident, off)
  (_,(i:_)) <- fmap (M.!key) get
  return $ Local ident off (Just i)
replaceVar other = return other

popVar :: SSAVar (Maybe Integer) -> State RenameState ()
popVar (Local ident off _) = do
  let key = (ident, off)
  modify $ M.adjust (second tail) key

-- moving data types from SIF to SSA
varSIF2SSA (Register loc) = Just $ Reg loc
varSIF2SSA (Stack var off) = Just $ Local var off (Nothing :: Maybe Integer)
varSIF2SSA _ = Nothing

assignsToSIF (SIFInstruction _ (SideEffect (Move _ x))) = varSIF2SSA x
assignsToSIF (SIFInstruction x (Unary{})) = Just $ Reg x
assignsToSIF (SIFInstruction x (Binary{})) = Just $ Reg x
assignsToSIF _ = Nothing

operandSIF2SSA oper = maybe (Val oper) Var (varSIF2SSA oper)

opcodeSIF2SSA (SideEffect (Move val _)) = Copy $ operandSIF2SSA val
opcodeSIF2SSA opc = SIF $ fmap operandSIF2SSA opc

instructionSIF2SSA i@(SIFInstruction loc opc) = SSAInstruction loc (assignsToSIF i) (opcodeSIF2SSA opc)

--fromSSA :: CFG SSA -> CFG SIFInstruction
fromSSA = hole
{- general plan for converting from SSA to SIF
  1. convert all assignments to stack variables into assignments to registers.
     update uses of the stack variables into uses of the new registers.
     (uses of initial parameter vaules are not changed)
  2. TODO
-}

-- The SSA data structures
data SSAVar a = 
    Reg SIFLocation -- a register
  | Local SIFIdentifier SIFOffset a -- a local or parameter
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data SSAOperand a = 
    Var (SSAVar a) -- equivalent of Stack and Register
  | Val SIFOperand -- no Stack or Register uses
  deriving (Show, Functor, Foldable, Traversable)

data SSAOpcode a = 
    Phi (M.Map Vertex (SSAVar a))	  
  | SIF (SIFOpcode (SSAOperand a)) -- no uses of Move. either sets a register or has an effect
  | Copy (SSAOperand a) -- equivalent of Move, but also allows for copying into a register
  deriving (Show, Functor, Foldable, Traversable)

data SSAInstruction a = SSAInstruction SIFLocation (Maybe (SSAVar a)) (SSAOpcode a)
  deriving (Show, Functor, Foldable, Traversable)

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
  pretty (Local var _ a) = text (var ++ "$" ++ show a)

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
