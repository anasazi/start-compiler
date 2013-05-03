{-# LANGUAGE TupleSections #-}
module ConstantPropagation (constantPropagation) where

import SSA (SSA(..))
import IR
import ControlFlowGraph
import qualified Data.Map as M
import Data.List ((\\), union, nub)
import Data.Maybe

import Debug.Trace

constantPropagation :: SSA Program -> SSA Program
constantPropagation (SSA p@(Program uts ms gs is)) = SSA . Program uts ms gs . concatMap scc $ routines p

data LatticeCell = Bot | CI Integer | CB Bool | Top deriving (Eq, Show)

meet Top x = x
meet Bot _ = Bot
meet (CI a) (CI b) | a == b = CI a | otherwise = Bot
meet (CI _) _ = Bot
meet (CB a) (CB b) | a == b = CB a | otherwise = Bot
meet (CB _) _ = Bot

{- 
    Variables are either registers or SSA locals.
    Registers are defined in the instruction with the same number as themselves

    SSA Locals are defined either as a parameter or in a move instruction

    When we use a variable, we just lookup that instruction and operate accordingly
    Theoretically, we will never need to eval one of the parameters, just look up the value
-}
data Variable = Register Integer | Local VarOperand Integer deriving (Eq, Ord, Show)
-- ssa edge
type SSAEdge = Instruction
-- variables yet to be processed
type SSAWorkList = [SSAEdge]
-- knowledge of constant values for variables
type Value = M.Map Variable LatticeCell

-- cfg node
newtype Node = Node Integer deriving (Eq, Ord, Show)
-- cfg edge
type FlowEdge = (Node, Node)
-- edges yet to be processed
type FlowWorkList = [FlowEdge]
-- knowledge of executability of flow edges
type Executable = M.Map FlowEdge Bool

-- NB could run into an issue where the src node isn't actually the node in the CFM
exec :: Executable -> FlowEdge -> Bool
exec ex (src,tar) = {-trace "exec"-} ex M.! (src, tar)

{-
exec :: Routine -> Executable -> FlowEdge  -> Bool
exec r x (Node n) = (x M.!) . blockIAmIn . graphToMap . cfg $ r
    where 
    blockIAmIn :: ControlFlowMap -> Node
    blockIAmIn cfm = Node . fst . head . filter f . M.toList $ cfm
    f :: (Integer, ([Instruction], [Integer])) -> Bool
    f = (n `elem`) . (map iloc) . fst . snd
    iloc (Instruction n _ _) = n
-}

-- the instruction that defined the variable
-- NB: will FAIL if you try to look up the definition of a parameter
def :: Routine -> Variable -> Instruction
def r (Register v) = head . filter ((==v) . iloc) $ r
    where iloc (Instruction n _ _) = n
def r (Local v 0) = error "No instruction defines initial values of SSA local variables"
def r (Local v s) = head . filter f $ r
    where f i = case i of Instruction _ (B Move _ (SSAVar v s)) _ -> True ; _ -> False

-- the instructions were a variable is used
use :: Routine -> Variable -> [Instruction]
use r (Register v) = filter (hasRegOperand v) r
use r (Local v s) = filter (hasLocalOperand v s) r

i2v :: Instruction -> Maybe Variable
i2v (Instruction _ (B Move _ (SSAVar v s)) _) = Just $ Local v s
i2v (Instruction n (Phi _ _) _) = Just $ Register n
i2v (Instruction n _ (Just _)) = Just $ Register n
i2v _ = Nothing

isdef :: Instruction -> Bool
isdef i = case i2v i of (Just _) -> True ; Nothing -> False

isCondBr :: Instruction -> Bool
isCondBr (Instruction _ (B Blbc _ _) _) = True
isCondBr (Instruction _ (B Blbs _ _) _) = True
isCondBr _ = False

isJump :: Instruction -> Bool
isJump (Instruction _ (U Br _) _) = True
isJump (Instruction _ (B Blbc _ _) _) = True
isJump (Instruction _ (B Blbs _ _) _) = True
isJump _ = False

data CondBrResult = Jump | Fall

condBrResult :: Instruction -> Bool -> CondBrResult
condBrResult (Instruction _ (B Blbc _ _) _) t = if not t then Jump else Fall
condBrResult (Instruction _ (B Blbs _ _) _) t = if t then Jump else Fall

condBrJump (Instruction _ (B Blbc _ (Const (L x))) _) = x
condBrJump (Instruction _ (B Blbs _ (Const (L x))) _) = x

hasLocalOperand v s (Instruction _ op _) =
    case op of
	Phi x subs -> x == v && s `elem` (map snd subs)
	U _ (SSAVar v s) -> True
	B _ (SSAVar v s) _ -> True
	B Move _ (SSAVar v s) -> False -- this is a definition
	B _ _ (SSAVar v s) -> True
	Ter _ (SSAVar v s) _ _ -> True
	Ter _ _ (SSAVar v s) _ -> True
	Ter _ _ _ (SSAVar v s) -> True
	_ -> False

hasRegOperand v (Instruction _ op _) =
    case op of
	U _ (Const (R v)) -> True
	B _ (Const (R v)) _ -> True
	B _ _ (Const (R v)) -> True
	Ter _ (Const (R v)) _ _ -> True
	Ter _ _ (Const (R v)) _ -> True
	Ter _ _ _ (Const (R v)) -> True
	_ -> False

-- evaluate the instruction that defines a variable
-- NB: will FAIL if you try to eval a parameter (SSA local with sub = 0), but that should never be called
--eval :: Routine -> Value -> Executable -> Variable -> LatticeCell
eval :: Routine -> Value -> Executable -> Instruction -> LatticeCell
--eval r val ex var =
eval r val ex inst =
    let --inst = def r var 
	me = Node . head . M.keys . M.filter (any (== inst) . fst) . graphToMap . cfg $ r
    in
    --case op $ def r var of
    case op inst of
	-- Definition of a SSA local var case
	B Move src _ -> valof src
	-- Definitions of registers
	Phi v ss -> foldl meet Top . map (({-trace "eval"-} val M.!) . Local v . snd) . filter ((exec ex) . ((,me)) . Node . fst) $ ss
	U Neg s -> case valof s of Bot -> Bot ; Top -> Top ; CI x -> CI $ negate x
	B Add a b -> case (valof a, valof b) of (CI x, CI y) -> CI $ x + y ; (x,y) -> meet x y
	B Sub a b -> case (valof a, valof b) of (CI x, CI y) -> CI $ x - y ; (x,y) -> meet x y
	B Mul a b -> 
	    case (valof a, valof b) of -- we can handle annihilators right now
		(CI x, CI y) -> CI $ x * y
		(CI 0, _) -> CI 0
		(_, CI 0) -> CI 0
		(Top, Bot) -> CI 0 -- Top could eventually be lowered to 0, so we don't go to Bot
		(x,y) -> meet x y
	B Div a b -> case (valof a, valof b) of (CI x, CI y) -> CI $ x `div` y ; (x, y) -> meet x y
	B Mod a b -> case (valof a, valof b) of (CI x, CI y) -> CI $ x `mod` y ; (x, y) -> meet x y
	B Cmpeq a b -> case (valof a, valof b) of (CI x, CI y) -> CB $ x == y ; (x, y) -> meet x y
	B Cmple a b -> case (valof a, valof b) of (CI x, CI y) -> CB $ x <= y ; (x, y) -> meet x y
	B Cmplt a b -> case (valof a, valof b) of (CI x, CI y) -> CB $ x  < y ; (x, y) -> meet x y
	_ -> Bot -- we can't determine anything else
    where op (Instruction _ x _) = x
	  valof operand = 
	    case operand of
		-- references to variables are resolved by just looking it up
		-- the (Var ...) case should never occur while in SSA form
		Const (R src)	    -> {-trace "valof - R"-} val M.! (Register src)
		SSAVar srcv srcs    -> {-trace "valof - SSAVar"-} val M.! (Local srcv srcs)
		-- copying a constant just results in that constant
		Const (C x)	-> CI x
		Const (A _ x)	-> CI x
		Const (SF _ x)	-> CI x
		Const (T _ x)	-> CI x
		-- we don't what these are
		Const GP -> Bot
		Const FP -> Bot
		Const (L _) -> Bot
		Const (DF _) -> Bot

variables :: Routine -> [Variable]
variables = map makeVariable . filter isAssignment
    where
    isAssignment (Instruction _ (Phi _ _) _) = True -- phi nodes
    isAssignment (Instruction _ (B Move _ _) _) = True -- ssa vars
    isAssignment (Instruction _ _ (Just _)) = True -- things that generate values
    isAssignment _ = False
    makeVariable (Instruction _ (B Move _ (SSAVar tarv tars)) _) = Local tarv tars
    makeVariable (Instruction n _ _) = Register n

params :: Routine -> [Variable]
params = nub . map makeVariable . filter isParam
    where
    isParam (Instruction _ (B Move (SSAVar _ _) _) _) = True
    isParam _ = False
    makeVariable (Instruction _ (B Move (SSAVar v s) _) _) = Local v s

nodes :: Routine -> [Node]
nodes = map Node . M.keys . graphToMap . cfg

edges :: Routine -> [FlowEdge]
edges = concatMap (\(k,ss) -> map ((Node k,) . Node) ss) . M.toList . M.map snd . graphToMap . cfg

scc :: Routine -> Routine
scc r = cleanup r $ scc' r cfm fw2 ssaw1 val1 ex0 
    where 
    isEnter (Instruction _ (U Enter _) _) = True
    isEnter _ = False
    cfm = graphToMap . cfg $ r
    start = Node . fst . head . filter (any isEnter . fst . snd) . M.toList $ cfm
    suc = let (Node x) = start in map Node . snd $ {-trace "scc"-} cfm M.! x
    val0 = M.fromList . map (,Top) $ (params r) ++ (variables r)
    ex0 = M.fromList . map (,False) . edges $ r
    (val1, ssaw1, fw1) = visitBlock r cfm val0 ex0 start False
    fw2 = if length suc == 1 then map (start,) suc else []

cleanup :: Routine -> (Value, Executable) -> Routine
cleanup r0 (val,ex) = r6
    where
    cfm0 = graphToMap . cfg $ r0
    deadEdges = M.keys $ M.filter not ex
    r1 = foldl removeDeadEdge r0 deadEdges
    cfm1 = graphToMap . cfg $ r1
    deadBlocks = M.keys cfm0 \\ M.keys cfm1
    r2 = foldl (removeDeadBlock cfm0 {-need old data-}) r1 deadBlocks
    -- all dead edges cleaned up. now for propagating values
    -- if there are any phi's with only one incoming val, we can just do an assignment
    r3 = singlePhisAreCopy r2
    -- if a variable has a constant value, then we can just replace all uses of it by a constant and remove the definition
    r4 = foldl replaceConstVar r3 $ M.toList val
    -- if we're do an operation with an identity, we an just copy the non-identity value. (i.e. replace all uses)
    r5 = replaceIdentities r4
    -- dead code elimination (by routine)
    r6 = deadCodeElimination r5
	-- critical: store, checknull/checktype/checkbounds?, stdynamic, write, wrl, enter, ret, call/param, entrypc
    -- cleaning up the cfg comes for free when we reconstruct it
    -- we're already coalescing by our way of propagation

deadCodeElimination :: Routine -> Routine
deadCodeElimination r = sweep r mark
    where
    cfm0 = graphToMap . cfg $ r
    mark :: M.Map Instruction Bool
    mark = 
	let iscrit = map (\i -> (i, isCritical i)) r 
	in mark' (M.fromList iscrit) (map fst . filter snd $ iscrit)
    mark' :: M.Map Instruction Bool -> [Instruction] -> M.Map Instruction Bool
    mark' marked [] = marked
    mark' marked (i@(Instruction n op _):is) =
	let (Node blk) = blockWith cfm0 n
	    pre = preds (mapToGraph cfm0) blk
	    ends = map (\p -> last . fst $ {-trace "mark' - ends"-} cfm0 M.! p) pre
	    brs = filter isJump ends
	    unmrk = filter (\b -> not $ {-trace "mark' - unmrk"-} marked M.! b) brs
	    marked' = M.union (M.fromList $ map (,True) unmrk) marked
	    work' = union unmrk is --unmrk ++ is
	    f xs = M.union (M.fromList $ map (,True) xs) marked'
	    g xs = union xs work' --xs ++ work'
	    h :: [Operand] -> [Instruction]
	    h os = map (def r) $ concatMap toVar os
	    j os = let x = h os in mark' (f x) (g x)
	in
	case op of 
	    U _ a -> j [a]
	    B Move a b ->  j [a]
	    B _ a b ->  j [a,b]
	    Ter _ a b c -> j [a,b,c]
	    _ -> j []
    sweep :: Routine -> M.Map Instruction Bool -> Routine
    sweep r m = concatMap (\i -> if {-trace "sweep"-} m M.! i then [i] else [])  r
    toVar :: Operand -> [Variable]
    toVar (SSAVar _ 0) = []
    toVar (SSAVar v s) = [Local v s]
    toVar (Const (R r)) = [Register r]
    toVar _ = []
    isCritical (Instruction _ op _) = 
	case op of
	    Z Wrl -> True
	    Z Entrypc -> True
	    U Checknull _ -> True
	    U Call _ -> True
	    U Write _ -> True
	    U Enter _ -> True
	    U Ret _ -> True
	    U Param _ -> True
	    U Br _ -> True
	    B Store _ _ -> True
	    B Checktype _ _ -> True
	    B Checkbounds _ _ -> True
	    B Blbc _ _ -> True
	    B Blbs _ _ -> True
	    Ter Stdynamic _ _ _ -> True
	    _ -> False

replaceIdentities :: Routine -> Routine
replaceIdentities r = concatMap h r
    where
    h (Instruction n _ _) | M.member n m = []
    h (Instruction n (U op a) t) = [Instruction n (U op (g a)) t]
    h (Instruction n (B Move a b) t) = [Instruction n (B Move (g a) b) t]
    h (Instruction n (B op a b) t) = [Instruction n (B op (g a) (g b)) t]
    h (Instruction n (Ter op a b c) t) = [Instruction n (Ter op (g a) (g b) (g c)) t]
    h i = [i]
    g (Const (R x)) | M.member x m = {-trace "replaceIdentities - g"-} m M.! x
    g x = x
    m = M.fromList $ concatMap f r
    f (Instruction n (B Add (Const (C 0)) x) _) = [(n,x)]
    f (Instruction n (B Add x (Const (C 0))) _) = [(n,x)]
    f (Instruction n (B Sub x (Const (C 0))) _) = [(n,x)]
    f (Instruction n (B Mul (Const (C 1)) x) _) = [(n,x)]
    f (Instruction n (B Mul x (Const (C 1))) _) = [(n,x)]
    f (Instruction n (B Div x (Const (C 1))) _) = [(n,x)]
    f _ = []

-- if the lattice cell is not a constant, then nothing happens
replaceConstVar :: Routine -> (Variable, LatticeCell) -> Routine
replaceConstVar r (v,lc) =
    case (v,lc) of
	(_, Top) -> r
	(_, Bot) -> r
	(v, CI i) -> map (replaceUse (v,i)) r
	(v, CB b) -> map (replaceUse (v, if b then 1 else 0)) r
    where
    replaceUse sub i@(Instruction n op t) = 
	case op of
	    Phi _ _ -> i
	    Z _ -> i
	    U uop a -> Instruction n (U uop (rO sub a)) t
	    B Move a b -> Instruction n (B Move (rO sub a) b) t
	    B bop a b -> Instruction n (B bop (rO sub a) (rO sub b)) t
	    Ter top a b c -> Instruction n (Ter top (rO sub a) (rO sub b) (rO sub c)) t
    rO :: (Variable, Integer) -> Operand -> Operand
    rO (Register var,val) (Const (R r)) | var == r = Const (C val)
    rO (Local vo sub,val) (SSAVar v s) | vo == v && sub == s = Const (C val)
    rO _ o = o

singlePhisAreCopy :: Routine -> Routine
singlePhisAreCopy ((Instruction n1 (Phi v1 [(src,s1)]) _):(Instruction n2 (B Move (Const (R r1)) v2) _):is) | n1 == r1 = (Instruction n1 (B Move (SSAVar v1 s1) v2) Nothing) : singlePhisAreCopy is
singlePhisAreCopy (i:is) = i : singlePhisAreCopy is
singlePhisAreCopy [] = []

removeDeadBlock :: ControlFlowMap -> Routine -> Integer -> Routine
removeDeadBlock cfm r blk = filter keep r
    where code = fst $ {-trace "removeDeadBlock"-} cfm M.! blk
	  inBlock i = i `elem` code
	  keep = not . inBlock

unnode (Node x) = x

removeDeadEdge :: Routine -> FlowEdge -> Routine
removeDeadEdge r (src,tar) = 
    let cfm = graphToMap . cfg $ r
	srcBr = let (Node x) = blockWith cfm (unnode src) in last . fst $ {-trace {-"removeDeadEdge - srcBr"-} (show (cfm, x))-} cfm M.! x
	srcLanding = {-trace ("srcLanding " ++ show srcBr)-} condBrJump srcBr
	jumpDead = blockWith cfm srcLanding == tar
	srcBr' = Instruction (iloc srcBr) (U Br (Const (L srcLanding))) Nothing
	fixSrc = if not (isCondBr srcBr) || jumpDead 
		 then filter (\i -> iloc i /= iloc srcBr) 
		 else map (\i -> if i == srcBr then srcBr' else i)
	tarPhis = let (Node x) = tar in filter isPhi $ fst $ {-trace "removeDeadEdge - tarPhis"-} cfm M.! x
	tarPhis' = let (Node x) = src in \(Instruction n (Phi v ss) t) -> Instruction n (Phi v (filter (\(pred,sub) -> pred == x) ss)) t 
	fixTar = if isCondBr srcBr && jumpDead
		 then map (\i -> if i `elem` tarPhis then tarPhis' i else i)
		 else id
    in fixTar . fixSrc $ r

visitBlock :: Routine -> ControlFlowMap -> Value -> Executable -> Node -> Bool -> (Value, SSAWorkList, [FlowEdge])
visitBlock r cfm val ex node visited =
    let code = let (Node x) = node in fst $ {-trace "visitBlock"-} cfm M.! x
	val' = foldl (visitBlockPhi r ex) val code
	(val'', ssaw', fw') = if visited then (val', [], []) else foldl (visitBlockExpr r cfm ex) (val, [], []) code
    in (val'', ssaw', fw')

visitBlockPhi :: Routine -> Executable -> Value -> Instruction -> Value
visitBlockPhi r ex val instr | isPhi instr = visitPhi r val ex instr
			     | otherwise = val

visitBlockExpr :: Routine -> ControlFlowMap -> Executable -> (Value, SSAWorkList, FlowWorkList) -> Instruction -> (Value, SSAWorkList, FlowWorkList)
visitBlockExpr r cfm ex (val, ssaw, fw) instr | isPhi instr = (val, ssaw, fw)
					      | otherwise = let (val', ssaw', fw') = visitExpr r cfm val ex instr in (val', ssaw ++ ssaw', fw ++ fw')

visitPhi :: Routine -> Value -> Executable -> Instruction -> Value
visitPhi r val ex instr = M.insert (Register $ iloc instr) (eval r val ex instr) val

visitExpr :: Routine -> ControlFlowMap -> Value -> Executable -> Instruction -> (Value, SSAWorkList, FlowWorkList)
visitExpr r cfm val ex instr = 
    let nblk = blockWith cfm (iloc instr)
	(Node blk) = nblk
	end = last . fst $ {-trace "visitExpr - end"-} cfm M.! blk
	(Just var) = i2v instr
	changed = {-trace "visitExpr - changed"-} val M.! var /= valof
	ssaw = if isdef instr && changed then use r var else []
	valof = eval r val ex instr
	controlsBr = isCondBr end && end `elem` (use r var)
	brpredict = case valof of Bot -> map (Node blk,) sucs ; CB truth -> whichone (condBrResult instr truth)
	jumpsto = blockWith cfm $ condBrJump instr
	whichone res = case res of Jump -> [(nblk, jumpsto)]; Fall -> map (nblk,) . filter (/=jumpsto) $ sucs
	sucs = map Node . snd $ {-trace "visitExpr - sucs"-} cfm M.! blk
	fw = if isdef instr && changed && not controlsBr then [] else brpredict
	-- controls branch = is the last instruction in block a branch and does it use the variable defined here?
	val' = if isdef instr && changed then M.insert var valof val else val
    in (val', ssaw, fw)

isPhi (Instruction _ (Phi _ _) _) = True
isPhi _ = False

blockWith :: ControlFlowMap -> Integer -> Node
blockWith cfm n = Node . fst . head . M.toList . M.filter (\(b,_) -> any ((==n) . iloc) b) $ cfm

iloc (Instruction n _ _) = n

blockVisited :: Executable -> Node -> Bool
blockVisited ex n = or . M.elems . M.filterWithKey (\(_,x) _ ->  x == n) $ ex

scc' :: Routine -> ControlFlowMap -> FlowWorkList -> SSAWorkList -> Value -> Executable -> (Value, Executable)
scc' r _ [] [] val ex = (val, ex)
scc' r cfm [] (useof:ssaw) val ex 
    | isPhi useof = let val' = visitPhi r val ex useof in scc' r cfm [] ssaw val' ex
    | blockVisited ex (blockWith cfm (iloc useof)) = 
	let (val', ssaw', fw') = visitExpr r cfm val ex useof in scc' r cfm fw' (ssaw ++ ssaw') val' ex
    | otherwise = (val, ex)
scc' r cfm (edge:fw) ssaw val ex 
    | exec ex edge = scc' r cfm fw ssaw val ex
    | otherwise = 
	let tar = snd edge
	    suc = let (Node x) = tar in snd $ {-trace "scc'"-} cfm M.! x
	    ex' = M.insert edge True ex
	    (val', ssaw', fw') = visitBlock r cfm val ex' tar (blockVisited ex tar)
	    fw'' = let (Node x) = tar in if length suc == 1 then map ((Node x,) . Node) suc else []
	in scc' r cfm (fw ++ fw' ++ fw'') (ssaw ++ ssaw') val' ex'
