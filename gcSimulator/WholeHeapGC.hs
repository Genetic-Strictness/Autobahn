{-# LANGUAGE BangPatterns #-}
module WholeHeapGC where

import Machine
import EtParserNoFields
--import qualified Data.ByteString.Lazy.Char8 as L
import qualified SmallIntSet as SmallS
import qualified Data.IntSet as S
import qualified Data.IntMap as M
--import qualified Data.Char as C
import qualified Graph as G
--import qualified Data.Bits as Bits
import qualified Data.Foldable as F
--import Prelude hiding (fmap)
--import System (getArgs)
import Data.List as List
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.IntMultiSet as MS
import Debug.Trace (trace)
{--type Frame = (Int,S.IntSet)
type Stack = Seq.Seq Frame
type StackMap = M.IntMap Stack--}

type Frame = (Int,SmallS.SmallIntSet)
type Stack = Seq.Seq Frame
type StackMap = M.IntMap Stack


push::Int->Int->StackMap->StackMap
push !method !thread !stacks =  M.insert thread ( (method,SmallS.empty) Seq.<| (f (M.lookup thread stacks))) stacks
			   where
				f (Just !frames) = frames
				f Nothing        = Seq.empty


pop::Int->Int->StackMap->StackMap
pop !method !thread !stacks = M.update f  thread  stacks
			   where
				f frames = if Seq.null frames then
						Nothing
					   else 
						--Drop all the leaded frames not corrsepodinging to 'method'
						--Then drop that one as well.
						Just $!  Seq.drop 1 $! Seq.dropWhileL (\(m,_) -> m /= method) frames
	
addRoot::Int->Int->StackMap->StackMap
addRoot !r !thread !stacks =  M.update f  thread  stacks 
			   where
				f frames  
					| Seq.null frames   = Just Seq.empty
					| headHasRoot r	    = Just frames
					| otherwise         = let (!mid, !roots)  = frames `Seq.index` 0 
	        						  !beheadedSeq   = Seq.drop 1 frames  in
		     						    Just $! (mid, SmallS.insert r roots) Seq.<| beheadedSeq
					where
					  headHasRoot::Int->Bool
					  headHasRoot r =  let (_,roots) = frames `Seq.index` 0
							   in
							     r `SmallS.member` roots 

class Machine m => CollectableMachine m where
  stacks::m->StackMap
  heap::m->G.Graph
  roots::m->MS.IntMultiSet
  replaceHeap::G.Graph->m->m
  incMarks::Int->m->m --Increment the number of marks
  


wholeHeapCollect::CollectableMachine m => m -> m
wholeHeapCollect m =  trace "Whole heap collection " $ replaceHeap heap' $! incMarks  (S.size markedSet) m --m{heap=heap',marks= marks m + S.size markedSet} 
		     where
			!heap' = sweep markedSet $! (heap m)
			mark::S.IntSet->S.IntSet->G.Graph->S.IntSet
			mark black grey heap =  if grey == S.empty then
					  	  black
						else
						  mark black' grey' heap

						where
						   black'::S.IntSet
						   !black' = S.foldl' (\m o -> S.insert o m) black grey 

						   grey'::S.IntSet
						   !grey' = S.foldl'  (\s i -> s `S.union` neighbors i) S.empty $ S.filter (not.isBlack) grey

						   neighbors::Int->S.IntSet
						   neighbors n = S.fromList $ (fromMaybe []) $ G.neighbors n heap

						   isBlack::Int->Bool
						   isBlack x = S.member x black
		
			markedSet = mark S.empty (getRoots m) $! (heap m)
	
			sweep::S.IntSet->G.Graph->G.Graph
			sweep marked heap  = S.fold G.delNode heap (G.nodes heap S.\\ marked)
			


getRoots::CollectableMachine m => m->S.IntSet
getRoots m =  staticRoots `S.union` M.fold (S.union) S.empty (M.map stackRoots $! (stacks m) )
	     where
		--get the roots for each individual stack
		stackRoots::Stack->S.IntSet
		stackRoots s = F.foldl' (\s1 small-> s1 `S.union` SmallS.toIntSet small) (S.empty)  (fmap ( \(_,roots)->roots) s)
		staticRoots = MS.toSet $! roots m

			
{--
main = do
	args <- getArgs
	contents <- L.readFile (args !! 0)
	print $! simulate $! map (f.readRecord) $! L.lines  contents
	
	where
	      f::Maybe (Record,L.ByteString) -> Record
	      f  (Just (r,_))   = r
	      f      _        = error "We are all going to die."
--}
