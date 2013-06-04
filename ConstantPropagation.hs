{-# LANGUAGE FlexibleInstances #-}
module ConstantPropagation 
( constantPropagation
) where

import BasicBlock
import ControlFlowGraph
import StaticSingleAssignment
import SIF
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Maybe

data Hole = Hole
hole = undefined

data Value = Number Integer | Logic Bool deriving (Eq, Show)
data Constancy = Variable | Const Value | Unknown deriving (Eq, Show)

instance Monoid Constancy where
  mempty = Unknown
  Unknown `mappend` x = x
  x `mappend` y | x == y = x
  x `mappend` y | x /= y = Variable
  Variable `mappend` _ = Variable

newtype Knowledge = K (M.Map (SSAVar Integer) Constancy) deriving (Eq, Show)
-- initial locals have variable constancy
value _ (Local _ _ _ 0) = Variable
value (K k) var = k M.! var
learn (K k) var con = K $ M.insert var con k

-- don't take conditional branches into account
simpleConstantPropagation cfg = 
  let defs = M.fromList . fmap (\(SSAInstruction _ (Just var) opc) -> (var, opc)) . filter (\(SSAInstruction _ tar _) -> isJust tar) . fromBlocks . linearize $ cfg
      vars = M.keys defs
      -- non-initial variables have unknown constancy
      k0 = K $ M.fromList [ (var, Unknown) | var <- vars ]
      step k = K $ M.fromList [ (var, eval k (defs M.! var)) | var <- vars ]
      kEnd = fixEq step k0
  in hole

constantPropagation :: CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
constantPropagation = hole

fixEq f v | v == v'   = v
	  | otherwise = fixEq f v'
  where v' = f v

newOpcode k (Phi inc) =
  case eval k (Phi inc) of
    Const (Number n) -> Copy (Val (Constant n))
    Const (Logic True) -> Copy (Val (Constant 1))
    Const (Logic False) -> Copy (Val (Constant 0))
    Variable -> Phi inc
newOpcode k (SIF sif) = hole
newOpcode k (Copy val) =
  case eval k val of
    Const (Number n) -> Copy (Val (Constant n))
    Const (Logic True) -> Copy (Val (Constant 1))
    Const (Logic False) -> Copy (Val (Constant 0))
    Variable -> Copy val

class Eval a where
  eval :: Knowledge -> a -> Constancy

instance Eval (SSAVar Integer) where
  eval k var = value k var

instance Eval SIFOperand where
  eval _ Global = Variable
  eval _ Frame = Variable
  eval _ (Constant num) = Const $ Number num
  eval _ (Address _ off) = Const $ Number off
  eval _ (StaticField _ off) = Const $ Number off
  eval _ (DynamicField _) = Variable
  eval _ (Register _) = error "No instances of Register in SSA form"
  eval _ (Stack _ _) = error "No instances of Stack in SSA form"
  eval _ (Type _ size) = Const $ Number size
  eval _ (Label loc) = Const $ Number loc

instance Eval (SSAOperand Integer) where
  eval k (Var var) = eval k var
  eval k (Val val) = eval k val

instance Eval (SIFOpcode (SSAOperand Integer)) where
  eval k (SideEffect _) = error "cannot evaluate a side effect"
  eval k (Unary op input _) = evalUnary op (eval k input)
  eval k (Binary op left right _) = evalBinary op (eval k left) (eval k right)
  eval k (Branch _ _) = error "cannot evalutate a branch"

evalUnary Neg (Const (Number n)) = Const $ Number (negate n)
evalUnary _ input = input

evalBinary Add (Const (Number l)) (Const (Number r)) = Const . Number $ l + r
evalBinary Sub (Const (Number l)) (Const (Number r)) = Const . Number $ l - r

evalBinary Mul Unknown Variable = Const $ Number 0
evalBinary Mul (Const (Number 0)) right = Const $ Number 0
evalBinary Mul left (Const (Number 0)) = Const $ Number 0
evalBinary Mul (Const (Number l)) (Const (Number r)) = Const . Number $ l * r

evalBinary Div (Const (Number l)) (Const (Number r)) = Const . Number $ l `div` r
evalBinary Mod (Const (Number l)) (Const (Number r)) = Const . Number $ l `mod` r

evalBinary Equal (Const (Number l)) (Const (Number r)) = Const . Logic $ l == r
evalBinary LessEqual (Const (Number l)) (Const (Number r)) = Const . Logic $ l <= r
evalBinary Less (Const (Number l)) (Const (Number r)) = Const . Logic $ l < r

evalBinary Istype _ _ = Variable
evalBinary Checktype _ _ = Variable
evalBinary LoadDyanmic _ _ = Variable

evalBinary _ left right = left <> right
{-
evalBinary _ _ Unknown = Unknown
evalBinary _ Unknown _ = Unknown
evalBinary _ _ Variable = Variable
evalBinary _ Variable _ = Variable
-}

instance Eval (SSAOpcode Integer) where
  eval k (Phi inc) = F.foldMap (eval k) inc
  eval k (SIF sif) = eval k sif
  eval k (Copy val) = eval k val

instance Eval (SSAInstruction Integer) where
  eval k (SSAInstruction loc Nothing opc) = error "cannot evalute a non-assignment"
  eval k (SSAInstruction loc (Just tar) opc) = eval k opc
