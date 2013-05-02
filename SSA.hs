module SSA where

import IR
import ControlFlowGraph
import Dominator
import Data.List (minimumBy, find, mapAccumL)
import Data.Graph
import Data.Ord
import qualified Data.Set as S
import qualified Data.Map as M

newtype NonSSA a = NonSSA a
newtype PhiPlaced a = PhiPlaced a 
newtype Renamed a = Renamed a
newtype SSA a = SSA a 
instance Functor SSA where
    fmap f (SSA x) = SSA (f x)

type Variable = VarOperand
type InstrID = Integer
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

phiRoutine :: InstrID -> NonSSA Routine -> (InstrID, PhiPlaced ControlFlowMap)
phiRoutine nextInstrID (NonSSA r) = coerce . foldl phiVariable (nextInstrID, cfm0) $ vars
    where cfm0 = NonSSA . graphToMap . cfg $ r
	  coerce (y,NonSSA x) = (y,PhiPlaced x)
	  vars = S.toList $ variables r

phiVariable :: (InstrID, NonSSA ControlFlowMap) -> Variable -> (InstrID, NonSSA ControlFlowMap)
phiVariable (nextInstrID, (NonSSA cfm)) v = coerce $ helper nextInstrID S.empty initWork initW cfm
    where
    initW = assigned cfm v
    initWork = initW
    coerce (a,b) = (a, NonSSA b)
    place :: (InstrID, ControlFlowMap) -> CFNode -> (InstrID, ControlFlowMap)
    place (id,cfm) y = 
	let (block, succ) = cfm M.! y
	    block' = (Instruction id (Phi v []) Nothing) : 
		     (Instruction (id+1) (B Move (Const (R id)) (Var v)) Nothing) : 
		     block
	    in (id+2, M.insert y (block', succ) cfm)
    helper :: InstrID -> NodeSet -> NodeSet -> NodeSet -> ControlFlowMap -> (InstrID, ControlFlowMap)
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
	  s0 = foldl (\m v -> M.insert v [0] m) M.empty vars
	  cfg = mapToGraph cfm
	  entry = toKey . find containsEnter . map (cfgNodeLU cfg) . vertices . cfgGraph $ cfg
	  toKey (Just (_,x,_)) = x
	  containsEnter (x,_,_) = any isEnter x
	  isEnter (Instruction _ (U Enter _) _) = True
	  isEnter _ = False
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
    renameInstr state i@(Instruction _ (Phi _ _) _) = (state, i)
    renameInstr state i@(Instruction _ (Z _) _) = (state, i)
    renameInstr state (Instruction l (U uop a) mt) =
	let a' = renameRHS state a
	in (state, Instruction l (U uop a') mt)
    renameInstr state (Instruction l (B Move rhs lhs) mt) =
	let rhs' = renameRHS state rhs
	    (state', lhs') = renameLHS state lhs
	in (state', Instruction l (B Move rhs' lhs') mt)
    renameInstr state (Instruction l (B bop a b) mt) =
	let a' = renameRHS state a
	    b' = renameRHS state b
	in (state, Instruction l (B bop a' b') mt)
    renameInstr state (Instruction l (Ter top a b c) mt) =
	let a' = renameRHS state a
	    b' = renameRHS state b
	    c' = renameRHS state c
	in (state, Instruction l (Ter top a' b' c') mt)
    renameRHS (c,s) (Var vo) = SSAVar vo (top s vo)
    renameRHS (c,s) o = o
    renameLHS (c,s) (Var vo) = let (i,c',s') = getI (c,s) vo in ((c',s'), SSAVar vo i)
    getI (c,s) v = let i = c M.! v in (i, M.update (const (Just $ i+1)) v c, M.update (Just . (i:)) v s)
    top s v = let (x:_) = s M.! v in x
    {- mark phi nodes of successors in control flow graph -}
    markSuccPhis acc = foldl markPhis acc succ -- for each successor Y of X
    -- for each phi function F in Y
    markPhis (c,s,cfm) y = (c,s, M.insert y (map (markPhiOfVar s) $ fst $ cfm M.! y, snd $ cfm M.! y) cfm)
    -- add the top of S(V) to F
    markPhiOfVar s (Instruction loc (Phi v subs) mt) = 
	let (sub:_) = s M.! v
	in Instruction loc (Phi v ((x,sub):subs)) mt
    markPhiOfVar _ i = i -- do not change non-phi instructions
    {- recursively search children in dominance tree -}
    searchChildren acc = foldl renameSearch acc kids
    {- clean up the stacks -}
    popOld acc = foldl (\(c,s,cfm) v ->  (c, pop v s, cfm)) acc $ map lhs . filter isAssignment $ code
    pop v s = M.insert v (tail $ s M.! v) s

-- need to update jump targets because of phi nodes
fixJumps :: Renamed ControlFlowMap -> SSA ControlFlowMap
fixJumps (Renamed cfm) = SSA . M.map (updateBlock cs) $ cfm
    where cs = newHeadsOfBlocks (Renamed cfm)

updateElements cs = M.map (\(b,ls) -> (updateBlock cs b, updateSuccs cs ls))

updateSuccs cs = map (updateLocation cs)

updateBlock cs (b,suc) = (map (updateInstruction cs) b, suc)

updateInstruction cs (Instruction l op mt) = Instruction l (updateOpcode cs op) mt

updateOpcode cs o@(Phi _ _) = o
updateOpcode cs o@(Z _) = o
updateOpcode cs (U uop a) = U uop (updateOperand cs a)
updateOpcode cs (B bop a b) = B bop (updateOperand cs a) (updateOperand cs b)
updateOpcode cs (Ter top a b c) = Ter top (updateOperand cs a) (updateOperand cs b) (updateOperand cs c)

updateOperand cs (Const (L x)) = Const . L $ updateLocation cs x
updateOperand _ o = o

updateLocation cs x = M.findWithDefault x x cs

-- for each node, find the new starting instruction
newHeadsOfBlocks :: Renamed ControlFlowMap -> M.Map Integer Integer
newHeadsOfBlocks (Renamed cfm) = M.map (first . fst) cfm
    where first ((Instruction x _ _):_) = x

-- lineraize the control flow graph
-- TODO if we fix the IDs of control flow nodes, then we will need to change this
linearize :: ControlFlowMap -> Routine
linearize cfm = concat . map fst . map snd . M.toAscList $ cfm 

-- this just produces the CFGs because the current CFG construction won't work with SSA stuff
programToSSA :: NonSSA Program -> SSA [ControlFlowGraph]
programToSSA = SSA . map f . map fixJumps . renameProgram . phiProgram
    where f (SSA cfm) = mapToGraph cfm

{-
routineToSSA :: NonSSA Routine -> SSA ControlFlowGraph
routineToSSA (NonSSA r) = fmap mapToGraph . fixJumps . renameRoutine . snd $ phiRoutine nextInstrID (NonSSA r)
    where nextInstrID = (+1) . maximum . map iloc $ r
	  iloc (Instruction loc _ _) = loc
-}

-- we need the old program for the type, method, and global declarations.
-- once I fix the CFG construction, then we can change it
-- TODO assumes that the method list and cfg list match up
programFromSSA :: Program -> SSA [ControlFlowGraph] -> NonSSA [Instruction]
programFromSSA (Program _ ms _ _) (SSA cfgs) = coalesce . mapAccumL routineFromSSA next $ work
    where cfms = map graphToMap cfgs
	  work = zipWith (\m cfm -> (m, SSA cfm)) ms cfms
	  next = (+1) . maximum . map (maximum . (map (maximum . map iloc . fst)) . M.elems) $ cfms
	  coalesce = NonSSA . concat . map (\(NonSSA x) -> x) . snd

routineFromSSA :: InstrID -> (Method, SSA ControlFlowMap) -> (InstrID, NonSSA Routine)
routineFromSSA next (m, SSA cfm) = 
    let bbs0 = map fst . M.elems $ cfm
        (v2r, bbs1) = set2reg m bbs0 -- step 1
	bbs2 = use2reg v2r bbs1 -- step 2
	(offs, bbs3) = offsetsAndEnter m bbs2 -- step 3
	cfm2 = M.fromList $ zipWith (\(k,(_,s)) b -> (k,(b,s))) (M.toList cfm) bbs3
	(next2, cfm3) = explodePhi v2r offs next cfm2 -- step 4
	r = linearize cfm3 -- step 5
    in (next2, NonSSA r)

type SSAVar = (VarOperand, Integer)
type Reg = Integer
type SSAVarToReg = M.Map SSAVar (Reg, Type)
type Phi = (Reg, VarOperand, [(CFNode, Integer)])
type Offset = Integer
type PhiOffsets = M.Map Phi Offset

iloc (Instruction l _ _) = l

isPhi (Instruction _ (Phi _ _) _) = True
isPhi _ = False

target (Instruction _ (B Move _ (SSAVar v i)) _) = (v,i)
target _ = error "not a ssa move"

typeOfVar :: Method -> VarOperand -> Type
typeOfVar (Method _ _ vars) (SV name offset) = third . toJust . find match $ vars
    where toJust (Just x) = x
	  toJust Nothing = error "toJust in typeOfVar"
	  match (x,y,z) = name == x && y == offset
	  third (a,b,c) = c

-- step 1a: convert phi assignment into a register operation
-- aka remove the move instruction after the phi
phiSet2Reg :: Method -> SSAVarToReg -> BasicBlock -> (SSAVarToReg, BasicBlock)
phiSet2Reg m v2r (i1:i2:is) 
    | isPhi i1 = 
	let (v,s) = target i2
	    t = typeOfVar m v
	    (v2r', is') = phiSet2Reg m (M.insert (v,s) (iloc i1, t) v2r) is 
	in (v2r', i1:is')
    | otherwise = let (v2r', is') = phiSet2Reg m v2r (i2:is) in (v2r', i1:is')
phiSet2Reg m v2r [i] = (v2r, [i])
phiSet2Reg m v2r [] = (v2r, [])

-- step 1b: convert assignments to ssa variables into register computations
ssaSet2Reg :: Method -> SSAVarToReg -> BasicBlock -> (SSAVarToReg, BasicBlock)
ssaSet2Reg m v2r is = mapAccumL ssaSet2Reg' v2r $ is
    where ssaSet2Reg' v2r (Instruction n (B Move x (SSAVar v s)) _) = (M.insert (v,s) (n,typeOf v) v2r,add0 n x v)
	  ssaSet2Reg' v2r i = (v2r, i)
	  typeOf v = typeOfVar m v
	  add0 n x v = Instruction n (B Add x (Const (C 0))) (Just $ typeOf v)

-- step 1
set2reg :: Method -> [BasicBlock] -> (SSAVarToReg, [BasicBlock])
set2reg m bs = (\(v2r,bs') -> mapAccumL (ssaSet2Reg m) v2r bs') $ mapAccumL (phiSet2Reg m) M.empty bs

-- step 2: covert all non-phi uses of SSA variables into uses of the registers instead
use2reg :: SSAVarToReg -> [BasicBlock] -> [BasicBlock]
use2reg v2r bbs = map (ssaUse2Reg v2r) bbs

ssaUse2Reg :: SSAVarToReg -> BasicBlock -> BasicBlock
ssaUse2Reg v2r = map ssaUse2Reg'
    where u2r (SSAVar v s) = Const (R $ fst (v2r M.! (v,s)))
	  u2r o = o
          ssaUse2Reg' (Instruction n opc t) =
	    let opc' = case opc of
			 U op a -> U op (u2r a)
			 B Move a b -> B Move (u2r a) b
			 B op a b -> B op (u2r a) (u2r b)
			 Ter op a b c -> Ter op (u2r a) (u2r b) (u2r c)
			 _ -> opc
	    in Instruction n opc' t

-- step 3a: create a map from phi nodes to offsets starting after the last local variable
phiOffsets :: Method -> [BasicBlock] -> PhiOffsets
phiOffsets (Method _ _ vars) blocks = M.fromList $ zip (allPhi blocks) allOffsets
    where 
    offset (_,x,_) = x
    start = if null vars then (-4) else offset $ minimumBy (comparing offset) vars
    allOffsets = enumFromThen start (start - 4)

allPhi :: [BasicBlock] -> [Phi]
allPhi = concatMap (concatMap keepPhi)
    where keepPhi (Instruction n (Phi v ss) _) = [(n,v,ss)]
	  keepPhi _ = []

-- step 3b: update enter instruction with new stack size
updateEnter :: Integer -> [BasicBlock] -> [BasicBlock]
updateEnter i = map (map f)
    where f (Instruction n (U Enter (Const (C x))) _) = Instruction n (U Enter (Const (C (x + i)))) Nothing
	  f i = i

-- step 3
offsetsAndEnter :: Method -> [BasicBlock] -> (PhiOffsets, [BasicBlock])
offsetsAndEnter m bs = let po = phiOffsets m bs in (po, updateEnter (toInteger . M.size $ po) bs)

-- step 4a: for each phi node, insert a move at the end of each predecessor
-- other interpretation: for each block, insert moves at the end for the phi nodes in the successors
insertPhiMoves :: SSAVarToReg -> PhiOffsets -> InstrID -> ControlFlowMap -> (InstrID, ControlFlowMap)
insertPhiMoves v2r phiOff next cfm = combine . mapAccumL (insertMoveBlock v2r phiOff) next $ work
    where combine (next', bs') = (next', M.fromList $ zipWith (\(k,(_,ss)) b -> (k,(b,ss))) (M.toList cfm) bs')
	  work = map (\(k,(b,ss)) -> (k,b,succPhis cfm k)) . M.toList $ cfm

insertMoveBlock :: SSAVarToReg -> PhiOffsets -> InstrID -> (CFNode,BasicBlock,[Phi]) -> (InstrID, BasicBlock)
insertMoveBlock v2r offs next (me,block,phis) = foldl insertMove (next,block) work
    where sourceVar (_,v,ps) = (v,snd . (\(Just x) -> x) . find ((==me) . fst) $ ps)
	  sourceReg = fst . (v2r M.!)
	  source = sourceReg . sourceVar
	  target = (offs M.!)
	  work = map (\p -> (source p, target p)) phis

succPhis :: ControlFlowMap -> CFNode -> [Phi]
succPhis cfm x = concatMap getPhiNodes . map (fst . (cfm M.!)) . snd $ cfm M.! x

insertMove :: (InstrID, BasicBlock) -> (Reg, Offset) -> (InstrID, BasicBlock)
insertMove (n,b) (r,o) =
    let (pre,suf) = splitAtBranch b
	i = Instruction n (B Move (Const (R r)) (Var (SV "__ssa_util" o))) Nothing
    in (n+1, pre ++ (i : suf))

splitAtBranch :: BasicBlock -> ([Instruction],[Instruction])
splitAtBranch b = 
    let x = (init b, [last b])
	y = (b, [])
	(Instruction _ op _) = last b in
    case op of
	U Br _ -> x
	B Blbc _ _ -> x
	B Blbs _ _ -> x
	_ -> y

getPhiNodes :: BasicBlock -> [Phi]
getPhiNodes = allPhi . (:[])

-- step 4b: replace each phi node with "add <var> 0 :<type>"
replacePhi :: SSAVarToReg -> PhiOffsets -> ControlFlowMap -> ControlFlowMap
replacePhi v2r offs cfm = M.map (\(b,s) -> (replacePhiBlock v2r offs b, s)) cfm

replacePhiBlock :: SSAVarToReg -> PhiOffsets -> BasicBlock -> BasicBlock
replacePhiBlock v2r offs bb = map f bb
    where f (Instruction n (Phi v subs@((_,s):_)) _) = Instruction n (B Add (Const (C 0)) (Var (SV "__ssa_util" (offs M.! (n,v,subs))))) (Just $ t v s)
	  f i = i
	  t v s = snd (v2r M.! (v,s))

-- step 4
explodePhi :: SSAVarToReg -> PhiOffsets -> InstrID -> ControlFlowMap -> (InstrID, ControlFlowMap)
explodePhi v2r offs next cfm = g . f $ cfm
    where f = insertPhiMoves v2r offs next
	  g (next,cfm) = (next, replacePhi v2r offs cfm)

{-
Plan for a program getting out of SSA:
0. get the next instruction id for the program, then for each routine do the following:
    1. convert assignments to SSA variables into register computations. 
       record a mapping from SSA variable to register
	-- NO assignments to SSA variables remain (including the ones following phi nodes)
    2. convert all non-phi uses of SSA variables into uses of those registers instead
	-- NO uses of SSA variables remain except for phi nodes
    3. for simplicity, we're going to make a new stack variable for every phi node. 
       Create a map from phi nodes to offsets (beginning after the last existing local var)
       Update the "enter" instruction with the new stack frame size
	-- we could do much better (max across nodes { number of distinct phi nodes in successors } )
	-- but I don't want to figure out how to do the assignment to variables
    4. for each phi node, insert a move to the assigned stack variable at the end of the predecessor
       replace each phi node with something like "add <var> 0" to read the stack variable into the register
	-- moves for phi nodes should be the last instructions before a jump, if one exists
	-- NO remnants of SSA remain!
    5. linearize the routine
	-- the starting location in the method declaration is still accurate because it still points to "enter"
6. concantenate the routines
-}

-- TODO need to read the initial values of stack variables into registers upon method entry (i.e. we need to map <var>$0 to a register)
