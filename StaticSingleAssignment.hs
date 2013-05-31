{-# LANGUAGE NoMonomorphismRestriction #-}
module StaticSingleAssignment 
( toSSA, fromSSA
, SSAInstruction
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
import Control.Arrow hiding ((<+>))

data Hole = Hole
hole = undefined

toSSA :: CFG SIFInstruction -> State SIFLocation (CFG (SSAInstruction SIFLocation))
toSSA cfg = do
  let restructured = fmap instructionSIF2SSA cfg
  fmap renamePhis $ insertPhis restructured

-- inserting phis where needed
insertPhis :: CFG (SSAInstruction ()) -> State SIFLocation (CFG (SSAInstruction ()))
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
renamePhis :: CFG (SSAInstruction ()) -> CFG (SSAInstruction SIFLocation)
renamePhis cfg =
  let vars = S.toList . S.unions . fmap stackVars . M.elems . blocks $ cfg
      state = M.fromList [ (v, (0, [])) | v <- vars ]
  in evalState (rename cfg (entry cfg)) state

rename :: CFG (SSAInstruction ()) -> Vertex -> State (M.Map (SSAVar ()) (Integer, [Integer])) (CFG (SSAInstruction SIFLocation))
rename cfg v = do
  -- get block v
  let b = blocks cfg M.! v
  -- generate a name for the target of each phi node
  -- replace variables used and generate names for variables assigned to in block
  -- update the phi nodes of CFG successors
  -- recurse into children in dominator tree
  -- pop variables defined in block
  hole

genName var = do
  (i,_) <- fmap (M.!var) get
  modify $ M.adjust ((+1) *** (i:)) var
  return $ fmap (const i) var

replaceVar var = do
  (_,(i:_)) <- fmap (M.!var) get
  return $ fmap (const i) var

popVar var = do
  modify $ M.adjust (second tail) var

-- moving data types from SIF to SSA
varSIF2SSA (Register loc) = Just $ Reg loc
varSIF2SSA (Stack var off) = Just $ Local var off ()
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

-- The SSA data structures
data SSAVar a = 
    Reg SIFLocation -- a register
  | Local SIFIdentifier SIFOffset a -- a local or parameter
  deriving (Show, Eq, Ord)
instance Functor SSAVar where
  fmap f (Local ident off x) = Local ident off $ f x
  fmap f (Reg loc) = Reg loc

data SSAOperand a = 
    Var (SSAVar a) -- equivalent of Stack and Register
  | Val SIFOperand -- no Stack or Register uses
  deriving Show
instance Functor SSAOperand where
  fmap f (Var ssavar) = Var $ fmap f ssavar
  fmap f (Val sifoper) = Val sifoper

data SSAOpcode a = 
    Phi (M.Map Vertex (SSAVar a))	  
  | SIF (SIFOpcode (SSAOperand a)) -- no uses of Move. either sets a register or has an effect
  | Copy (SSAOperand a) -- equivalent of Move, but also allows for copying into a register
  deriving Show
instance Functor SSAOpcode where
  fmap f (Phi inc) = Phi $ fmap (fmap f) inc
  fmap f (SIF sif) = SIF $ fmap (fmap f) sif
  fmap f (Copy oper) = Copy $ fmap f oper

data SSAInstruction a = SSAInstruction SIFLocation (Maybe (SSAVar a)) (SSAOpcode a)
  deriving Show
instance Functor SSAInstruction where
  fmap f (SSAInstruction loc tar opc) = SSAInstruction loc (fmap (fmap f) tar) (fmap f opc)

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
