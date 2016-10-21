module NurseryGC where

import qualified WholeHeapGC as GS
import WholeHeapGC (push, pop)
import Data.Maybe (fromMaybe)
import qualified Graph as G
import qualified Data.IntSet as S
import qualified Data.IntMultiSet as MS
import EtParser
import Machine
import Control.Exception (assert)


{--This collector /just/ does a nursery collection, with not collection of the adult generation.
   Mostly it is for testing. --}


class Machine m => NurseryMachine m where
  nursery::m->S.IntSet
  addToNursery::Int->m->m
  clearNursery::m->m
  promote::Int->m->m
  promoteSet::S.IntSet->m->m  
  

simStep::Machine->Record->Machine
simStep m (Alloc oid size _ _) = --Check if we need a nursery collection.
					     --If so, promote; then if heap_limit exceeded, do a whole heap collection.

					     if doNurseryCollection then
				       	 	ncollectedMachine{alloc_bytes=alloc_bytes', nursery_collections = nursery_collections m + 1}
					     else
					        m{nursery=nursery', nursery_size = nursery_size', heap_size=heap_size', heap=heap', alloc_obj=alloc_obj'}
					     where
						nursery' = S.insert oid (nursery m)
						heap_size' = (heap_size m + size)
						heap'  =  G.insNode oid (heap m)
					        doNurseryCollection = nursery_size' > nursery_limit m
						nursery_size' = (nursery_size m) + size
						--(!collectedMachine,!newMarks) = wholeHeapCollect ma
					        ncollectedMachine = nurseryCollect m
						alloc_bytes' = (alloc_bytes m) + size
						alloc_obj' = (alloc_obj m) + 1


simStep m (Death oid) 		  = m{nursery=nursery', heap=heap'} 
				    where
					nursery' = S.delete oid (nursery m)
					heap'    = G.delNode oid (heap m)

simStep m (Update old origin new _ _) = m{heap=heap', roots=roots', rem_set=rem_set''}
				   where
					!heap' = G.insEdge origin new $! G.delEdge origin old (heap m)
					roots' = if origin == 0 then
							MS.insert new $! MS.delete old  (roots m)
						    else
							roots m
					
                                        {--Check of the new pointer is a adulut->nursery pointer, update rem set accordingly--}	
					rem_set' = if not $ origin `S.member` nursery m && new `S.member` nursery m then
							MS.insert new (rem_set m)
						   else
							rem_set m
					{--Check to see if the old target was in the remembered set and remove it. --}
					rem_set'' = if not $ origin `S.member` nursery m && old `S.member` nursery m then
							MS.delete old rem_set' {-- Removes only one copy; rem_set' is a MultISet--}
						    else
							rem_set'
								
							

simStep m (Entry mid _ tid) = m{stacks=stacks'}
				     where
					stacks'=push mid tid (stacks m)

simStep m (Exit mid _ tid) = m{stacks=stacks'}
				     where
					!stacks' = pop mid tid (stacks m)


simStep m (Root r t) = m{stacks=GS.addRoot r t (stacks m)} 


{-- TODO: Not sure if this is correct. --}
nurseryCollect::Machine->Machine
nurseryCollect m =  assert (promotions m <= alloc_obj m) $ m{heap=sweepedHeap, marks=(marks m + S.size marked), promotions=(promotions m + S.size marked), nursery=S.empty, rem_set=MS.empty, rem_set_origins=MS.empty, nursery_size=0 }
		    where
 	              -- nurserySearch roots heap = mark S.empty roots 
		   
                       marked = S.filter (\x->S.member x (G.nodes $ heap m) )  $mark S.empty (MS.toSet (roots m) `S.union` MS.toSet (rem_set m) `S.union` GS.getRoots m)
		       --Remove those things from the heap that are in the nursery, and unmarked
		       sweepedHeap = S.fold  G.delNode (heap m) $ S.filter (\x -> not $ S.member x marked) $ nursery m
		       

		       mark::S.IntSet->S.IntSet->S.IntSet
		       mark black grey = if grey == S.empty then
					   black
		            	         else
					   mark black' grey'
		        where
		          black'::S.IntSet
			  !black'  = S.fold (\o m -> S.insert o m) black grey
		          grey' = S.fold (S.union.filterBlack.nurseryNeighbors) S.empty black'
			 
			  filterBlack  = S.filter (\x -> not $ S.member x black)


			  nurseryNeighbors::Int->S.IntSet
		          nurseryNeighbors n = S.fromList $ filter (\x->S.member x (nursery m)) $  fromMaybe [] $ G.neighbors n (heap m)
		       
                       
