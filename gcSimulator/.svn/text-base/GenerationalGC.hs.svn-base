module GenerationalGC where


import  NurseryGC (nurseryCollect)
import qualified WholeHeapGC as GS 
import WholeHeapGC (push, pop)
import Data.IntSet as S
import Data.IntMultiSet as MS
import Machine
import EtParser
import Graph as G

simStep::Machine->Record->Machine
simStep m (Alloc oid size _ _) = wCollectedMachine
				   where
				      nursery' = S.insert oid (nursery m)
				      heap_size' = (heap_size m + size)
				      heap'  =  G.insNode oid (heap m)
				      doCollection = heap_size' > heap_limit m
				      doNurseryCollection = nursery_size' > nursery_limit m
				      nursery_size' = (nursery_size m) + size
				      --(!collectedMachine,!newMarks) = wholeHeapCollect ma
				      --ncollectedMachine = nurseryCollect m
				      alloc_bytes' = (alloc_bytes m) + size
				      alloc_obj' = (alloc_obj m) + 1

				      nCollectedMachine = if doNurseryCollection then
							    (nurseryCollect m){alloc_bytes=alloc_bytes', alloc_obj=alloc_obj', nursery_collections = nursery_collections m + 1}
							  else
							    m{nursery=nursery', nursery_size = nursery_size', heap_size=heap_size', heap=heap', alloc_bytes=alloc_bytes', alloc_obj=alloc_obj'}


				      wCollectedMachine = if doNurseryCollection && doCollection then
							    (GS.wholeHeapCollect nCollectedMachine){whole_heap_collections = whole_heap_collections nCollectedMachine + 1}
							  else
							    nCollectedMachine
							


simStep m (Death oid) 		  = m{nursery=nursery', heap=heap'} 
				    where
					nursery' = S.delete oid (nursery m)
					heap'    = G.delNode oid (heap m)

simStep m (Update old origin new _ _) = m{heap=heap', rem_set=rem_set'',roots = roots'}
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



