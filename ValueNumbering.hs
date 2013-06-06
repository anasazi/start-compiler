{-# LANGUAGE NoMonomorphismRestriction #-}
module ValueNumbering
( valueNumbering
) where

import InstructionSet
import Data.Maybe
import BasicBlock
import ControlFlowGraph
import StaticSingleAssignment
import SIF
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Arrow
import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Debug.Trace

data Hole = Hole
hole = undefined

valueNumbering :: CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
valueNumbering = dominatorValueNumberTechnique

type ValueNumber = SSAVar Integer
type Expr = SSAOpcode Integer
type HashTable = M.Map Expr ValueNumber
type Var = SSAVar Integer
type VN = M.Map Var ValueNumber
type VNState = (VN, HashTable)

getVarVN :: Var -> State VNState ValueNumber
getVarVN var = ((M.findWithDefault var var) . fst) <$> get

putVarVN :: Var -> ValueNumber -> State VNState ()
putVarVN var val = modify (first (M.insert var val))

getExprVN :: Expr -> State VNState (Maybe ValueNumber)
getExprVN expr = ((M.lookup expr) . snd) <$> get

putExprVN :: Expr -> ValueNumber -> State VNState ()
putExprVN expr val = modify (second (M.insert expr val))

dominatorValueNumberTechnique :: CFG (SSAInstruction Integer) -> CFG (SSAInstruction Integer)
dominatorValueNumberTechnique cfg = 
  let vnum = dvnt (M.empty, M.empty) cfg (entry cfg)
      vnum' = removeEmptyVertices vnum
  in fixJumps vnum'

fixJumps cfg =
  let jumping = M.filter (isJump . end) (blocks cfg)
      isLeap = edge (const True) (const False)
      f v b = 
	let (SSAInstruction loc Nothing (SIF (Branch x _))) = end b
	    sv = goesTo . head . filter isLeap . S.toList $ succs cfg v
	    s = label (blocks cfg M.! sv)
	in case x of
	  Jump -> SSAInstruction loc Nothing (SIF (Branch Jump (Val (Label s))))
	  IfZero test -> SSAInstruction loc Nothing (SIF (Branch (IfZero test) (Val (Label s))))
	  IfSet test -> SSAInstruction loc Nothing (SIF (Branch (IfSet test) (Val (Label s))))
      setBranch v b = fmap (\i -> if isJump i then f v b else i) b
      blocks' = M.mapWithKey setBranch jumping
  in mapBlocks (\v b -> M.findWithDefault b v blocks') cfg

dvnt :: VNState -> CFG (SSAInstruction Integer) -> Vertex -> CFG (SSAInstruction Integer)
dvnt vns cfg v = 
  let (is', vns') = runState (catMaybes <$> T.mapM process (body $ blocks cfg M.! v)) vns 
      cfg' = modifyNode (modifyBlock (const is')) v cfg
      children = S.toList . S.map goesTo $ succs cfg' v
      cfg'' = foldl (propagateToPhis (fst vns') v) cfg' children
      dominated = M.keys $ M.filter (maybe False (==v)) (idominators cfg'')
      cfg''' = foldl (dvnt vns') cfg'' dominated
  in cfg'''

propagateToPhis :: VN -> Vertex -> CFG (SSAInstruction Integer) -> Vertex -> CFG (SSAInstruction Integer)
propagateToPhis vn parent cfg child = modifyNode (fmap (overwritePhi vn parent)) child cfg

overwritePhi :: VN -> Vertex -> SSAInstruction Integer -> SSAInstruction Integer
overwritePhi vn parent (SSAInstruction loc tar (Phi inc)) =
  let old = inc M.! parent
      new = M.findWithDefault old old vn 
  in SSAInstruction loc tar $ Phi $ M.insert parent new inc
overwritePhi _ _ i = i

process :: SSAInstruction Integer -> State VNState (Maybe (SSAInstruction Integer))
process (SSAInstruction loc Nothing opc) = Just . SSAInstruction loc Nothing . SIF <$> 
  case opc of
    SIF (SideEffect effect) -> SideEffect <$> T.mapM update effect
    SIF (Branch branch target) -> Branch <$> T.mapM update branch <*> update target
    SIF NOP -> pure NOP
process i@(SSAInstruction loc (Just tar) opc) = do
  case opc of
    Phi inc ->
      if meaningless inc
	then do
	  putVarVN tar $ head (M.elems inc)
	  return Nothing
	else standardProcess loc tar (Phi inc)
    expr -> do
      expr <- simplify <$> overwrite expr
      standardProcess loc tar expr
  
-- if redundant expression then throw away
-- otherwise keep
standardProcess loc tar (SIF (Unary op arg typ)) | op `elem` [Isnull, Load, New, Newlist, Checknull] = do
  putVarVN tar tar
  return $ Just $ SSAInstruction loc (Just tar) (SIF (Unary op arg typ))
standardProcess loc tar (SIF (Binary op l r typ)) | op `elem` [Istype, Checktype, LoadDynamic] = do
  putVarVN tar tar
  return $ Just $ SSAInstruction loc (Just tar) (SIF (Binary op l r typ))
standardProcess loc tar expr = do
  mvn <- getExprVN expr
  case mvn of
    Just vn -> do
      putVarVN tar vn
      return Nothing
    Nothing -> do
      putVarVN tar tar
      putExprVN expr tar
      return $ Just $ SSAInstruction loc (Just tar) expr

meaningless :: M.Map Vertex (SSAVar Integer) -> Bool
meaningless inc = let (x:xs) = M.elems inc in all (==x) xs

update :: SSAOperand Integer -> State VNState (SSAOperand Integer)
update (Var var) = fmap Var (getVarVN var)
update oper = return oper

overwrite :: Expr -> State VNState Expr
overwrite (Copy val) = Copy <$> update val
overwrite (SIF sif) = SIF <$> T.mapM update sif
overwrite phi = error "overwrite should not be called on a phi node"

simplify :: Expr -> Expr
simplify sif@(SIF (Binary op left right typ)) = simplifyBinary op left right typ
simplify sif@(SIF (Unary op input typ)) = simplifyUnary op input typ
simplify expr = expr

zero = Val $ Constant 0
one = Val $ Constant 1
two = Val $ Constant 2

true = one
false = zero

only = const . Copy
commute op l r = SIF . Binary op (min l r) (max l r)

simplifyUnary :: SIFUnary -> SSAOperand Integer -> (SIFType -> Expr)
-- change -x into 0 - x so we can try to match it with other expressions
simplifyUnary Neg input = SIF . Binary Sub (Val (Constant 0)) input
-- we can't do anything else with unary ops
simplifyUnary op input = SIF . Unary op input

simplifyBinary :: SIFBinary -> SSAOperand Integer -> SSAOperand Integer -> (SIFType -> Expr)
simplifyBinary Add left right | left  == zero = only right
			      | right == zero = only left
			      | otherwise     = commute Add left right
simplifyBinary Sub left right | right == zero = only left
			      | right == left = only zero
			      -- | otherwise     = SIF . Binary Sub left right
simplifyBinary Mul left right | left  == two  = SIF . Binary Add right right
			      | right == two  = SIF . Binary Add left left
			      | left  == one  = only right
			      | right == one  = only left
			      | left  == zero = only zero
			      | right == zero = only zero
			      | otherwise     = commute Mul left right
simplifyBinary Div left right | right == one  = only left
			      | left  == right && right /= zero = only one
			      -- | otherwise     = SIF . Binary Div left right
simplifyBinary Mod left right | right == one  = only zero
			      -- | otherwise     = SIF . Binary Mod left right
simplifyBinary Equal left right | left == right = only true
				| otherwise = commute Equal left right
simplifyBinary LessEqual left right | left == right = only true
				    -- | otherwise = SIF . Binary LessEqual left right
simplifyBinary Less left right | left == right = only false
			       -- | otherwise = SIF . Binary Less left right
-- if we can't do anything, just return the input
simplifyBinary op left right = SIF . Binary op left right
