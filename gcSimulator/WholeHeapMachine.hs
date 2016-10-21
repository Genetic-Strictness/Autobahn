module WholeHeapMachine where
import Machine (Machine,Stack,Frame,StackMap,allocate,update,root,methodEntry,methodExit,death, gb,mb)
import WholeHeapGC (CollectableMachine, addRoot, push, pop, wholeHeapCollect)
import qualified WholeHeapGC as WH
import Data.IntSet as S
import Graph as G
import Data.IntMultiSet as MS
import Data.Sequence as Seq
import Data.IntMap as M

--A machine with the bare minimum to support whole heap collection
data WholeHeapMachine = WholeHeapMachine {
				     heap_size :: !Int
				   , heap_limit :: !Int
				   , heap :: !G.Graph
				   , roots :: !MS.IntMultiSet  {-- Only the static roots --}
			           , stacks :: !StackMap
				   , alloc_bytes :: !Int {--Number of bytes allocated --}
				   , alloc_obj :: !Int {--Number of objects allocated --}
				   , marks :: !Int --Just a count of marking done by the GC
				   , whole_heap_collections :: !Int
				   --, large :: !Int --Objects bigger than this size are considered "large"
				   --, large_objects :: !S.IntSet	
				   , time :: !Int --What time is it now?
				 } deriving (Show,Eq)

initMachine = WholeHeapMachine {
				      heap_size = 0
				    , heap_limit = 4*gb
				    , heap = G.empty
				    , roots = MS.empty
				    , stacks = M.empty
				    , alloc_bytes = 0
				    , alloc_obj = 0
				    , marks = 0
				    , whole_heap_collections = 0
				    , time = 0 
                   }

 
instance Machine WholeHeapMachine where
  allocate oid size _ _ !m = if doCollection then
				       	 	collectedMachine{alloc_bytes=alloc_bytes', alloc_obj= (alloc_obj m) + 1}
					     else
					        m{heap_size=heap_size', heap=heap', alloc_bytes=alloc_bytes', alloc_obj= (alloc_obj m) + 1}
					     where
						heap_size' = (heap_size m + size)
						heap'  =  G.insNode oid (heap m)
						doCollection = heap_size' > heap_limit m
						collectedMachine = wholeHeapCollect m
						alloc_bytes' = (alloc_bytes m) + size
						death !oid !m 		  = m{heap=heap'}
				    where
					heap'    = G.delNode oid (heap m)

  update !old !origin !new _ _ !m = m{heap=heap'}
				   where
					!heap' = G.insEdge origin new $! G.delEdge origin old (heap m)

  methodEntry !mid _ !tid !m = m{stacks=stacks'}
				     where
					stacks'=push mid tid (stacks m)

  methodExit  !mid _ !tid !m = m{stacks=stacks'}
				     where
					!stacks' = pop mid tid (stacks m)

  root !r !t              !m  = m{stacks=addRoot r t (stacks m)}


instance CollectableMachine WholeHeapMachine where
  stacks = WholeHeapMachine.stacks
  heap   = WholeHeapMachine.heap
  roots  = WholeHeapMachine.roots
  replaceHeap h m = m{heap=h}
  incMarks i m      = m{marks= (marks m)+i}
