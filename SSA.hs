module SSA where

import IR
import ControlFlowGraph
import Dominator
import Data.List (mapAccumL)
import Data.Graph
import qualified Data.Set as S
import qualified Data.Map as M

newtype NonSSA a = NonSSA a
newtype PhiPlaced a = PhiPlaced a 
newtype Renamed a = Renamed a
newtype SSA a = SSA a 
instance Functor SSA where
    fmap f (SSA x) = SSA (f x)

type Variable = VarOperand
type InstructionID = Integer
type CFNode = Integer
type NodeSet = S.Set CFNode
type Counters = M.Map Variable Integer
type Stacks = M.Map Variable [Integer]

variables :: [Instruction] -> S.Set Variable
variables = S.fromList . concatMap instructionVariables
    where
    instructionVariables (Instruction _ op _) = opcodeVariables op
    opcodeVariables (Phi v _) = [v]
    opcodeVariables (Z _) = []
    opcodeVariables (U _ a) = [a] >>= operandVariable
    opcodeVariables (B _ a b) = [a,b] >>= operandVariable
    opcodeVariables (Ter _ a b c) = [a,b,c] >>= operandVariable
    operandVariable (Var vo) = [vo]
    operandVariable _ = []

assigned :: ControlFlowMap -> Variable -> NodeSet
assigned cfm v = S.fromList . map key . filter contains $ nodes
    where key (_,k,_) = k
	  nodes = map (\(b,(a,c)) -> (a,b,c)) . M.assocs $ cfm
	  contains (b,_,_) = v `elem` blockAssignments b
	  blockAssignments = concatMap instructionAssignment
	  instructionAssignment (Instruction _ op _) = opcodeAssignment op
	  opcodeAssignment (B Move _ v) = operandVariable v
	  opcodeAssignment _ = []
	  operandVariable (Var vo) = [vo]
	  operandVariable _ = []

phiProgram :: NonSSA Program -> [PhiPlaced ControlFlowMap]
phiProgram (NonSSA prgm) = snd . mapAccumL phiRoutine nextInstrID . map NonSSA . routines $ prgm
    where nextInstrID = (+1) . maximum . map iloc $ is
	  iloc (Instruction loc _ _) = loc
	  (Program _ _ _ is) = prgm

phiRoutine :: InstructionID -> NonSSA Routine -> (InstructionID, PhiPlaced ControlFlowMap)
phiRoutine nextInstrID (NonSSA r) = coerce . foldl phiVariable (nextInstrID, cfm0) $ vars
    where cfm0 = NonSSA . graphToMap . cfg $ r
	  coerce (y,NonSSA x) = (y,PhiPlaced x)
	  vars = S.toList $ variables r

phiVariable :: (InstructionID, NonSSA ControlFlowMap) -> Variable -> (InstructionID, NonSSA ControlFlowMap)
phiVariable (nextInstrID, (NonSSA cfm)) v = coerce $ helper nextInstrID S.empty initWork initW cfm
    where
    initW = assigned cfm v
    initWork = initW
    coerce (a,b) = (a, NonSSA b)
    place :: (InstructionID, ControlFlowMap) -> CFNode -> (InstructionID, ControlFlowMap)
    place (id,cfm) y = 
	let (block, succ) = cfm M.! y
	    block' = (Instruction id (Phi v []) Nothing) : 
		     (Instruction (id+1) (B Move (Const (R id)) (Var v)) Nothing) : 
		     block
	    in (id+2, M.insert y (block', succ) cfm)
    helper :: InstructionID -> NodeSet -> NodeSet -> NodeSet -> ControlFlowMap -> (InstructionID, ControlFlowMap)
    helper next hasAlready work w cfm =
	if S.null w
	then (next, cfm)
	else let (x, w') = S.deleteFindMin w
		 y = (M.!x) . dominanceFrontier . mapToGraph $ cfm
		 y' = S.difference y hasAlready
		 y'' = S.difference y' work
		 hasAlready' = S.union hasAlready y'
		 work' = S.union work y''
		 (next', cfm') = S.foldl place (next, cfm) y'
	     in helper next' hasAlready' work' w' cfm'

renameProgram = map renameRoutine

renameRoutine :: PhiPlaced ControlFlowMap -> Renamed ControlFlowMap
renameRoutine (PhiPlaced cfm) = Renamed cfm'
    where (_,_,cfm') = renameSearch (c0,s0,cfm) entry
	  c0 = foldl (\m v -> M.insert v 0 m) M.empty vars
	  s0 = foldl (\m v -> M.insert v [] m) M.empty vars
	  cfg = mapToGraph cfm
	  (first:_) = topSort (cfgGraph cfg)
	  entry = (\(_,x,_) -> x) . (cfgNodeLU cfg) $ first
	  vars = S.toList . S.unions . map variables . map fst . M.elems $ cfm

isAssignment (Instruction _ (B Move _ (Var _)) _) = True
isAssignment (Instruction _ (Phi _ _) _) = True
isAssignment _ = False

renameSearch :: (Counters,Stacks,ControlFlowMap) -> CFNode -> (Counters, Stacks, ControlFlowMap)
renameSearch (c,s,cfm) x = popOld . searchChildren . markSuccPhis $ renameInBlock 
    where
    kids = S.toList $ children (immediateDominators (mapToGraph cfm)) x
    (code, succ) = cfm M.! x
    -- lhs of an assignment
    lhs (Instruction _ (B Move _ (Var v)) _) = v
    lhs (Instruction _ (Phi v _) _) = v
    lhs _ = error "not an assignment"
    {- rename normal instructions -}
    renameInBlock = arrange cfm $ mapAccumL renameInstr (c,s) code
    arrange cfm ((c,s),code') = (c, s, M.insert x (code', succ) cfm)
    renameInstr (c,s) i@(Instruction _ (Phi _ _) _) = ((c,s), i)
    renameInstr (c,s) i@(Instruction _ (Z _) _) = ((c,s), i)
    renameInstr state (Instruction l (U uop a) mt) = 
	let (state', a') = renameOperand state a in (state', Instruction l (U uop a') mt)
    renameInstr (c,s) (Instruction l (B Move (Var rhs) lhs) mt) = 
	let (sub:_) = s M.! rhs
	    rhs' = SSAVar rhs sub
	    (state', lhs') = renameOperand (c,s) lhs
	in (state', Instruction l (B Move rhs' lhs') mt)
    renameInstr state (Instruction l (B bop a b) mt) = 
	let (state', a') = renameOperand state a 
	    (state'', b') = renameOperand state' b 
	in (state'', Instruction l (B bop a' b') mt)
    renameInstr state (Instruction l (Ter top a b d) mt) = 
	let (state', a') = renameOperand state a 
	    (state'', b') = renameOperand state' b 
	    (state''', d') = renameOperand state'' d 
	in (state''', Instruction l (Ter top a' b' d') mt)
    renameOperand (c,s) (Const co) = ((c,s),Const co)
    renameOperand (c,s) (Var vo) = let (i,c',s') = getI (c,s) vo in ((c',s'), SSAVar vo i)
    getI (c,s) v = let i = c M.! v in (i, M.update (const (Just $ i+1)) v c, M.update (Just . (i:)) v s)
    {- mark phi nodes of successors in control flow graph -}
    markSuccPhis acc = foldl markPhis acc succ -- for each successor Y of X
    -- for each phi function F in Y
    markPhis (c,s,cfm) y = (c,s, M.insert y (map (markPhiOfVar s) $ fst $ cfm M.! y, snd $ cfm M.! y) cfm)
    -- add the top of S(V) to F
    markPhiOfVar s (Instruction loc (Phi v subs) mt) = 
	let (sub:_) = s M.! v
	in Instruction loc (Phi v (sub:subs)) mt
    markPhiOfVar _ i = i -- do not change non-phi instructions
    {- recursively search children in dominance tree -}
    searchChildren acc = foldl renameSearch acc kids
    {- clean up the stacks -}
    popOld acc = foldl (\(c,s,cfm) v ->  (c, pop v s, cfm)) acc $ map lhs . filter isAssignment $ code
    pop v s = M.insert v (tail $ s M.! v) s

-- TODO need to update jump targets because of phi nodes
fixJumps :: Renamed ControlFlowMap -> SSA ControlFlowMap
fixJumps (Renamed cfm) = undefined
-- for each node, find the new starting instruction
-- substitute old node values for the new
-- in each node, substitute old locations for the new
-- has the benefit of also updating the graph keys

programToSSA :: NonSSA Program -> SSA [ControlFlowGraph]
programToSSA = SSA . map f . map fixJumps . renameProgram . phiProgram
    where f (SSA cfm) = mapToGraph cfm

routineToSSA :: NonSSA Routine -> SSA ControlFlowGraph
routineToSSA (NonSSA r) = fmap mapToGraph . fixJumps . renameRoutine . snd $ phiRoutine nextInstrID (NonSSA r)
    where nextInstrID = (+1) . maximum . map iloc $ r
	  iloc (Instruction loc _ _) = loc

-- replace each phi node with move instructions in the predecessors
routineFromSSA :: SSA ControlFlowGraph -> NonSSA Routine
routineFromSSA (SSA cfg) = undefined
