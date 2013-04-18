{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
module Dominator where

import ControlFlowGraph
import Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ((>=>))

type Node = Integer
type NodeSet = S.Set Node
type Out = M.Map Node NodeSet
type In = M.Map Node NodeSet
type Doms = M.Map Node NodeSet
type IDom = M.Map Node (Maybe Node)

idom :: Doms -> IDom
idom !doms = M.mapWithKey (\k ds -> extract . imm k ds $! expansion ds) doms
    where
    extract = safeHead >=> return . S.findMax :: [NodeSet] -> Maybe Node
    safeHead ![] = Nothing
    safeHead !(x:_) = Just x
    imm !k !ds = filter (==(S.delete k ds)) :: [NodeSet] -> [NodeSet]
    expansion !ds = map (doms M.!) (S.toList ds) :: [NodeSet]

dominators :: ControlFlowGraph -> Doms
dominators !cfg = forwardDataFlow values entry transfer meet boundary pred
    where (!g,!nlu,!vlu) = (cfgGraph cfg, cfgNodeLU cfg, cfgVertexLU cfg)
	  key (_,!x,_) = x
	  values = map (key . nlu) . vertices $! g :: Values
	  entry = key . nlu $! 0 :: Entry
	  transfer = S.insert :: Transfer
	  meet = S.intersection :: Meet
	  boundary = S.singleton entry :: Boundary
	  pred = preds cfg :: Pred


type Values = [Node]
type Entry = Node
type Transfer = Node -> NodeSet -> NodeSet
type Meet = NodeSet -> NodeSet -> NodeSet
type Boundary = NodeSet
type Pred = Node -> [Node]
forwardDataFlow :: Values -> Entry -> Transfer -> Meet -> Boundary -> Pred -> Out
forwardDataFlow !vs !e !t !m !b !p = snd $! converge next (in0,out0)
    where in0 = M.fromList $! zip vs (repeat $! S.empty) :: In
	  out0 = M.insert e b $! M.fromList $! zip vs (repeat $! S.fromList vs) :: Out
	  next !(!inA,!outA) = (inStep inA outA, outStep inA outA) :: (In,Out)
	  inStep !inA !outA = M.mapWithKey (\k o -> if k == e then inA M.! e else foldl1 m (map (outA M.!) $! p k)) outA
	  outStep !inA !outA = M.mapWithKey (\k i -> if k == e then outA M.! e else t k i) inA
	  !(!inEnd,!outEnd) = converge next (in0, out0) :: (In,Out)

converge next init = fst $! until unchanged step (boot init)
    where boot !x = (x, next x)
          step !(_,!b) = (b, next b)
	  unchanged !(!a,!b) = a == b
