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

data Hole = Hole
hole = undefined

toSSA :: CFG SIFInstruction -> State SIFLocation (CFG (SSAInstruction SIFLocation))
toSSA cfg = do
  let restructured = fmap instructionSIF2SSA cfg
  let df = dominanceFrontier restructured
  let sv = fmap stackVars $ blocks restructured
  let insertionLocations v = S.unions . fmap (sv M.!) . M.keys . M.filter (S.member v) $ df
  let workToDo = M.fromList [ (v, insertionLocations v) | v <- vertices restructured ]
  let toBeInserted = hole -- list of instructions to insert at the start of the block (after enter/entrypc)
  let inserted = mapBlocks (\v b -> insertPhis (toBeInserted v) b) restructured
  hole

insertPhis = hole

stackVars = S.fromList . fmap target . filter isStackAssignment . body
  where isStackAssignment (SSAInstruction _ (Just (Local _ _ _)) _) = True
	isStackAssignment _ = False
	target (SSAInstruction _ (Just x) _) = x

varSIF2SSA (Register loc) = Just $ Reg loc
varSIF2SSA (Stack var off) = Just $ Local var off ()
varSIF2SSA _ = Nothing

assignsToSIF (SIFInstruction _ (SideEffect (Move _ x))) = varSIF2SSA x
assignsToSIF (SIFInstruction x (Unary _ _ _)) = Just $ Reg x
assignsToSIF (SIFInstruction x (Binary _ _ _ _)) = Just $ Reg x
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
data SSAOperand a = 
    Var (SSAVar a) -- equivalent of Stack and Register
  | Val SIFOperand -- no Stack or Register uses
  deriving Show
data SSAOpcode a = 
    Phi (M.Map Vertex (SSAVar a))	  
  | SIF (SIFOpcode (SSAOperand a)) -- no uses of Move. either sets a register or has an effect
  | Copy (SSAOperand a) -- equivalent of Move, but also allows for copying into a register
  deriving Show

data SSAInstruction a = SSAInstruction SIFLocation (Maybe (SSAVar a)) (SSAOpcode a)
  deriving Show

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
