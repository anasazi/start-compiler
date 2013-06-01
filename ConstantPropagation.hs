{-# LANGUAGE FlexibleInstances #-}
module ConstantPropagation 
( constantPropagation
) where

import ControlFlowGraph
import StaticSingleAssignment
import SIF
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Foldable as F

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

newtype Knowledge = K (M.Map (SSAVar Integer) Constancy)
value (K k) var = k M.! var
learn (K k) var con = K $ M.insert var con k

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

-- don't take conditional branches into account
simpleConstantPropagation = hole

constantPropagation :: CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
constantPropagation = hole
