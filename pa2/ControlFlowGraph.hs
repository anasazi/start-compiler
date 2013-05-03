module ControlFlowGraph 
( Routine
, BasicBlock
, ControlFlowGraph
, cfgGraph, cfgNodeLU, cfgVertexLU, cfg
, succs, preds
, routines, routinesWithHeaders
, graphToMap, mapToGraph, ControlFlowMap
) where

import IR
import Data.Graph
import Data.List (find, sort, nub, mapAccumL, (\\))
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
routines = map snd . routinesWithHeaders

routinesWithHeaders :: Program -> [(Method, Routine)]
routinesWithHeaders (Program _ ms _ is) = map g ms  --map f ms
  where 
  entries = map rloc ms
  extract y = takeWhile (not . (`elem` (filter (/= y) entries)) . iloc) . dropWhile ((/= y) . iloc) $ is 
  g m = (m, extract . rloc $ m)
  rloc = \(Method _ x _) -> x
  iloc = \(Instruction x _ _) -> x

-- starting instruction is a leader
start = (\(Just (Instruction n _ _)) -> n) . find isEnter
  where isEnter (Instruction _ (U Enter _) _) = True
	isEnter (Instruction _ (Z Entrypc) _) = True
	isEnter (Instruction _ (Phi _ _) _) = True
        isEnter _ = False

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
leaders r = nub $ start r : targets r ++ followers r

basicBlocks :: Routine -> [BasicBlock]
basicBlocks r = map getInstructions ls
  where ls = leaders r
	getInstructions l = takeWhile (not . (`elem` (filter (/= l) ls)) . iloc) . dropWhile ((/= l) . iloc) $ r
	iloc (Instruction n _ _) = n

-- edges of the control flow graph where each block is represented by the location of its leader
-- TODO need to add fallthrough in conditional branches
buildEdgeList :: Routine -> [BasicBlock] -> [(BasicBlock, Integer, [Integer])]
buildEdgeList r bbl = map (\b -> (b, label b, (fall r b) ++ jumps b)) bbl
    where label = (\(Instruction x _ _) -> x) . head :: BasicBlock -> Integer

fall :: Routine -> BasicBlock -> [Integer]
fall r b = if fallsOff b then fallthrough else []
    where (Instruction end _ _) = last b
	  iloc (Instruction n _ _) = n
	  fallthrough = map iloc . take 1 . drop 1 . dropWhile ((/=end) . iloc) $ r 

fallsOff bb = 
    case last bb of
	Instruction _ (U Br _) _ -> False
	Instruction _ (U Ret _) _ -> False
	otherwise -> True
		

-- labels of the blocks which a basic block exits to
jumps :: BasicBlock -> [Integer]
jumps bb = nub . sort . map target . filter isJump $ bb
    where isJump (Instruction _ (U Br _) _) = True
	  isJump (Instruction _ (B Blbc _ _) _) = True
	  isJump (Instruction _ (B Blbs _ _) _) = True
	  isJump _ = False
	  target (Instruction _ (U Br (Const (L t))) _) = t
	  target (Instruction _ (B Blbc _ (Const (L t))) _) = t
	  target (Instruction _ (B Blbs _ (Const (L t))) _) = t
	
-- build the control flow graph
cfg :: Routine -> ControlFlowGraph
cfg r = CFG . graphFromEdges $ el2
    where s = start r
	  el1 = buildEdgeList r . basicBlocks $ r
	  (g,nlu,vlu) = graphFromEdges el1
	  (Just sv) = vlu s
	  rvs = sort $ reachable g sv
	  rbbs = map ((\(n,_,_) -> n) . nlu) rvs
	  el2 = buildEdgeList r $ rbbs

