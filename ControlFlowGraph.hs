module ControlFlowGraph 
( Routine
, BasicBlock
, ControlFlowGraph
, cfgGraph, cfgNodeLU, cfgVertexLU, cfg
, succs, preds
, routines
, graphToMap, mapToGraph, ControlFlowMap
) where

import IR
import Data.Graph
import Data.List (sort, nub, mapAccumL, (\\))
import qualified Data.Map as M

type Routine = [Instruction]
type BasicBlock = [Instruction]
newtype ControlFlowGraph = CFG (Graph, Vertex -> (BasicBlock, Integer, [Integer]), Integer -> Maybe Vertex) 

instance Show ControlFlowGraph where
    show (CFG (g,_,_)) = show g

cfgGraph (CFG (g,_,_)) = g
cfgNodeLU (CFG (_,n,_)) = n
cfgVertexLU (CFG (_,_,k)) = k

type ControlFlowMap = M.Map Integer (BasicBlock, [Integer])

graphToMap :: ControlFlowGraph -> ControlFlowMap
graphToMap cfg = M.fromList . map rearrange . map (cfgNodeLU cfg) . vertices . cfgGraph $ cfg
    where rearrange (a,b,c) = (b,(a,c))

mapToGraph :: ControlFlowMap -> ControlFlowGraph
mapToGraph = CFG . graphFromEdges . map rearrange . M.toList
    where rearrange (b,(a,c)) = (a,b,c)

toJust (Just x) = x
toJust Nothing = error "tried toJust on Nothing!"

succs :: ControlFlowGraph -> Integer -> [Integer]
succs (CFG (g,nlu,vlu)) k = outs . nlu $ v
    where outs (_,_,x) = x
	  (Just v) = vlu k

preds :: ControlFlowGraph -> Integer -> [Integer]
preds (CFG (g,nlu,vlu)) k = extract . parents . edges $ g
    where key (_,x,_) = x
	  (Just v) = vlu k
	  parents = filter ((==v) . snd)
	  extract = map (key . nlu . fst)

routines :: Program -> [Routine]
routines (Program _ ms _ is) = map getInBounds bounds
  where 
  bounds = zip (map rloc ms) (drop 1 (map rloc ms) ++ [1 + iloc (last is)])
  getInBounds (a,b) = filter (\i -> iloc i >= a && iloc i < b) is
  rloc = \(Method _ x _) -> x
  iloc = \(Instruction x _ _) -> x

-- starting instruction is a leader
-- TODO not actually correct. entry point of routine may not be first instruction
start ((Instruction n _ _):_) = n
start [] = error "No starting instruction in an empty routine."

-- the targets of jumps are leaders
targets = map extract . filter keep
    where keep (Instruction _ (U Br _) _) = True
	  keep (Instruction _ (B Blbc _ _) _) = True
	  keep (Instruction _ (B Blbs _ _) _) = True
	  keep _ = False
	  extract (Instruction _ (U Br   (Const (L t))) _)                = t
	  extract (Instruction _ (B Blbc _              (Const (L t))) _) = t
	  extract (Instruction _ (B Blbs _              (Const (L t))) _) = t

-- instructions immediately after jumps are leaders
followers = map follower . filter (jump . fst) . pairup
    where pairup x = zip x (tail x)
	  jump (Instruction _ (U Br _) _) = True
	  jump (Instruction _ (U Ret _) _) = True
	  jump (Instruction _ (B Blbc _ _) _) = True
	  jump (Instruction _ (B Blbs _ _) _) = True
	  jump _ = False
	  follower (_,Instruction n _ _) = n

-- sorted list of unique leaders
leaders :: Routine -> [Integer]
-- TODO shouldn't sort since instructions may not be in order
leaders r = nub . sort $ start r : targets r ++ followers r

basicBlocks :: Routine -> [BasicBlock]
basicBlocks r = reverse . map reverse . snd $ foldl f (ls,[]) r
    where ls = leaders r
	  f (ls,[]) i = (ls,[[i]])
	  f ([l],b:bb) i = ([l],(i:b):bb)
	  f (l1:l2:ls,b:bb) i@(Instruction n _ _) | l1 <= n && n < l2 = (l1:l2:ls,(i:b):bb)
						  | n == l2 = (l2:ls,[i]:b:bb)

-- edges of the control flow graph where each block is represented by the location of its leader
-- TODO need to add fallthrough in conditional branches
buildEdgeList :: [BasicBlock] -> [(BasicBlock, Integer, [Integer])]
buildEdgeList bbl = map (\bb -> (bb, label bb, jumps begin end bb)) bbl
    where label = (\(Instruction x _ _) -> x) . head :: BasicBlock -> Integer
	  end = (\(Instruction x _ _) -> x) . last . last $ bbl :: Integer
	  begin = label . head $ bbl

-- labels of the blocks which a basic block exits to
jumps :: Integer -> Integer -> BasicBlock -> [Integer]
jumps minTarget maxTarget bb = nub . sort . (fall++) . map target . filter isJump $ bb
    where isJump (Instruction _ (U Br _) _) = True
	  isJump (Instruction _ (U Call (Const (L t))) _) = minTarget <= t && t <= maxTarget
	  isJump (Instruction _ (B Blbc _ _) _) = True
	  isJump (Instruction _ (B Blbs _ _) _) = True
	  isJump _ = False
	  target (Instruction _ (U Br (Const (L t))) _) = t
	  target (Instruction _ (U Call (Const (L t))) _) = t
	  target (Instruction _ (B Blbc _ (Const (L t))) _) = t
	  target (Instruction _ (B Blbs _ (Const (L t))) _) = t
	  end@(Instruction n _ _) = last bb
	  fall = if fallsOff && n+1 <= maxTarget then [n+1] else []
	  fallsOff = case end of
	    Instruction _ (U Br _) _ -> False
	    Instruction _ (U Ret _) _ -> False
	    otherwise -> True
	
-- build the control flow graph
cfg :: Routine -> ControlFlowGraph
cfg r = CFG . graphFromEdges $ el2
    where s = start r
	  el1 = buildEdgeList . basicBlocks $ r
	  (g,nlu,vlu) = graphFromEdges el1
	  (Just sv) = vlu s
	  rvs = sort $ reachable g sv
	  rbbs = map ((\(n,_,_) -> n) . nlu) rvs
	  el2 = buildEdgeList $ rbbs

