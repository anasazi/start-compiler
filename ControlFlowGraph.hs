module ControlFlowGraph 
( Routine
, BasicBlock
, ControlFlowGraph
, cfgGraph, cfgNodeLookup, cfgKeyLookup, cfg
-- temp for testing
, routines
, basicBlocks
) where

import IR
import Data.Graph
import Data.List (sort, nub, mapAccumL)

type Routine = [Instruction]
type BasicBlock = [Instruction]
newtype ControlFlowGraph = CFG (Graph, Vertex -> (BasicBlock, Integer, [Integer]), Integer -> Maybe Vertex)
cfgGraph (CFG (g,_,_)) = g
cfgNodeLookup (CFG (_,n,_)) = n
cfgKeyLookup (CFG (_,_,k)) = k

routines :: Program -> [Routine]
routines (Program _ ms _ is) = map getInBounds bounds
  where 
  bounds = zip (map rloc ms) (drop 1 (map rloc ms) ++ [1 + iloc (last is)])
  getInBounds (a,b) = filter (\i -> iloc i >= a && iloc i < b) is
  rloc = \(Method _ x _) -> x
  iloc = \(Instruction x _ _) -> x

-- starting instruction is a leader
start ((Instruction n _ _):_) = n
start [] = error "No starting instruction in an empty routine."

-- the targets of jumps are leaders
targets = map extract . filter keep
    where keep (Instruction _ (U Br _) _) = True
	  keep (Instruction _ (B Blbc _ _) _) = True
	  keep (Instruction _ (B Blbs _ _) _) = True
	  keep _ = False
	  extract (Instruction _ (U Br (L t)) _) = t
	  extract (Instruction _ (B Blbc _ (L t)) _) = t
	  extract (Instruction _ (B Blbs _ (L t)) _) = t

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
leaders r = nub . sort $ start r : targets r ++ followers r

basicBlocks :: Routine -> [BasicBlock]
basicBlocks r = reverse . map reverse . snd $ foldl f (ls,[]) r
    where ls = leaders r
	  f (ls,[]) i = (ls,[[i]])
	  f ([l],b:bb) i = ([l],(i:b):bb)
	  f (l1:l2:ls,b:bb) i@(Instruction n _ _) | l1 <= n && n < l2 = (l1:l2:ls,(i:b):bb)
						  | n == l2 = (l2:ls,[i]:b:bb)

-- edges of the control flow graph where each block is represented by the location of its leader
label :: [BasicBlock] -> [(BasicBlock, Integer, [Integer])]
label = undefined

-- build the control flow graph
cfg :: Routine -> ControlFlowGraph
cfg = CFG . graphFromEdges . label . basicBlocks
