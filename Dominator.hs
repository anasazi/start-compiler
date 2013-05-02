module Dominator 
( immediateDominators, ImmediateDominators
, dominators, Dominators
, dominanceFrontier, DominanceFrontier
, children
) where

import ControlFlowGraph
import Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sort)

newtype Loc = Loc Integer deriving (Eq, Ord, Show)
data RPO = RPO Integer Loc deriving (Eq, Show)
instance Ord RPO where
    compare (RPO a _) (RPO b _) = compare a b

newtype Doms = Doms (M.Map RPO (Maybe RPO)) deriving (Eq, Show)
newtype IDom = IDom (M.Map Loc (Maybe Loc)) deriving Eq
newtype ReversePostOrder = ReversePostOrder [RPO] deriving Show
newtype RPOPred = RPOPred (M.Map RPO [RPO]) deriving Show

toJust _ (Just x) = x
toJust msg Nothing = error msg

key (_,x,_) = x

rpo (RPO r _) = r

-- flip the signs since we flipped RPO #s
intersect :: Doms -> RPO -> RPO -> RPO
intersect ds@(Doms doms) b1 b2 | b1 == b2 = b1
			       | b1 >  b2 = intersect ds (toJust "intersect" $ doms M.! b1) b2
			       | b1 <  b2 = intersect ds b1 (toJust "intersect" $ doms M.! b2)

reversePostOrder :: ControlFlowGraph -> ReversePostOrder
reversePostOrder cfg = 
    ReversePostOrder . zipWith RPO [1..] . map (Loc . key . cfgNodeLU cfg) . topSort . cfgGraph $ cfg
    where f = Loc . key . cfgNodeLU cfg
	  g = cfgGraph cfg
	  ff (a,b) = (f a, f b)

rpoPred :: ControlFlowGraph -> ReversePostOrder -> RPOPred
rpoPred cfg (ReversePostOrder rpo) = RPOPred . M.fromList $ map buildPreds rpo
    where buildPreds c@(RPO _ (Loc v)) = (c, map buildRPO (preds cfg v)) 
          buildRPO p = head . filter (\(RPO _ (Loc v)) -> v == p) $ rpo

start :: ReversePostOrder -> RPO
start (ReversePostOrder order) = head order 
    where loc (RPO _ (Loc v)) = v

initDoms :: ReversePostOrder -> Doms
initDoms rpo@(ReversePostOrder order) = Doms . startNode $ otherNodes
    where n0 = start rpo
	  startNode = M.insert n0 (Just n0)
	  otherNodes = M.fromList $ zip order (repeat Nothing)

newDomsForLoc :: RPOPred -> Doms -> RPO -> Doms
newDomsForLoc (RPOPred pred) ds@(Doms doms) b = update . collapse . sort . processedOnly $ pred M.! b
    where update = Doms . (\v -> M.insert b v doms) :: Maybe RPO -> Doms
	  collapse [] = error "There should always be a processed predecessor." 
	  collapse (r:rs) = foldl f (Just r) rs
	  f ma b = ma >>= \a -> return $ intersect ds b a
	  processedOnly = filter (\x -> Nothing /= (doms M.! x))

newDoms :: ReversePostOrder -> RPOPred -> Doms -> Doms
newDoms rpo@(ReversePostOrder order) pred doms = foldl f doms order
    where f old b | b == start rpo = old
		  | otherwise  = newDomsForLoc pred old b

loopWhileChanged :: ReversePostOrder -> RPOPred -> Doms -> Doms
loopWhileChanged order pred old = if old == new then old else loopWhileChanged order pred new
    where new = newDoms order pred old

rpoToLoc :: RPO -> Loc
rpoToLoc (RPO _ n) = n

domsToIDom :: Doms -> IDom
domsToIDom (Doms doms) = IDom . M.mapKeys rpoToLoc . M.map (fmap rpoToLoc) $ doms

cooper :: ControlFlowGraph -> IDom
cooper cfg = domsToIDom $ loopWhileChanged rpo pred init
    where rpo = reversePostOrder cfg
	  pred = rpoPred cfg rpo
	  init = initDoms rpo

strip :: IDom -> M.Map Integer (Maybe Integer)
strip (IDom idom) = M.mapKeys val . M.map (fmap val) $ idom
    where val (Loc v) = v 

type ImmediateDominators = M.Map Integer (Maybe Integer)
type Dominators = M.Map Integer (S.Set Integer)
type DominanceFrontier = M.Map Integer (S.Set Integer)

immediateDominators :: ControlFlowGraph -> ImmediateDominators
immediateDominators = strip . cooper

dominators :: ImmediateDominators -> Dominators
dominators idoms = M.mapWithKey allDoms idoms
    where allDoms _ Nothing  = S.empty 
	  allDoms k (Just b) = if k == b then S.singleton b else S.insert b (allDoms b (idoms M.! b))

children :: ImmediateDominators -> Integer -> S.Set Integer
children idom x = S.fromList . filter (/=x) . M.keys . M.filter (==x) . collapse $ idom 
    where collapse = M.map toJust . M.filter (/= Nothing)
	  toJust (Just x) = x

topoNodes :: ControlFlowGraph -> [Integer]
topoNodes cfg = map (key . cfgNodeLU cfg) . topSort . cfgGraph $ cfg

dominanceFrontier :: ControlFlowGraph -> DominanceFrontier
dominanceFrontier cfg = foldl (oneDF cfg idom) init order
    where (_,entry,_) = cfgNodeLU cfg 0
	  -- this algorithm assumes the entry node has no idom. make it so.
	  idom = M.insert entry Nothing $ immediateDominators cfg
	  order = reverse . topoNodes $ cfg
	  init = M.empty

oneDF :: ControlFlowGraph -> ImmediateDominators -> DominanceFrontier -> Integer -> DominanceFrontier
oneDF cfg idom df0 x =  upDF cfg idom x . localDF cfg idom x $ init
    where init = M.insert x S.empty df0

localDF :: ControlFlowGraph -> ImmediateDominators -> Integer -> DominanceFrontier -> DominanceFrontier
localDF cfg idom x df = M.insert x (foldl f (df M.! x) (succs cfg x)) df
    where f dfx y = case idom M.! y of
			Nothing -> S.insert y dfx
			(Just iy) -> if iy /= x then S.insert y dfx else dfx

upDF :: ControlFlowGraph -> ImmediateDominators -> Integer -> DominanceFrontier -> DominanceFrontier
upDF cfg idom x df = M.insert x (S.foldl (\dfx z -> S.foldl f dfx (df M.! z)) (df M.! x) (children idom x)) df
    where f dfx y = case idom M.! y of
			Nothing -> S.insert y dfx 
			(Just iy) -> if iy /= x then S.insert y dfx else dfx
