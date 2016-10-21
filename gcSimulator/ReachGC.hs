{-# LANGUAGE BangPatterns #-}
module ReachGC where

--import  NurseryGC (nurseryCollect)
import qualified WholeHeapGC as GS 
import WholeHeapGC (push, pop,CollectableMachine,StackMap)
import qualified Data.IntSet as S
import Data.IntSet ((\\))
import qualified Data.IntMultiSet as MS
import qualified Data.IntMap as M
import Data.IntMap ((!))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe, fromJust)
import Machine
import EtParser
import Graph as G
import Data.List (find)
import Debug.Trace
import Data.List (foldl')
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Exception (assert)
import ObjectModel
import qualified Data.Sequence as Seq
import Data.Sequence (index, (|>), (<|))
import qualified Data.Foldable as F
import DeepSeqInstances
import Control.DeepSeq
import Data.Functor (fmap)

{--testMachine::Int->Machine->Machine
testMachine oid m = if oid `S.member` (nodes $ heap m) then
			m
		    else
                       error ("m's heap does not contain" ++ show oid)
--}

data ReachMachine = ReachMachine {
				     nursery :: !S.IntSet
				   , nursery_size :: !Int
				   , nursery_limit:: !Int
				   , rem_set :: !MS.IntMultiSet
				   , rem_set_origins :: !MS.IntMultiSet
				   , heap_size :: !Int
				   , heap_limit :: !Int
				   , heap :: !G.Graph
				   , roots :: !MS.IntMultiSet  {-- Only the static roots --}
			           , stacks :: !StackMap
				   , alloc_bytes :: !Int {--Number of bytes allocated --}
				   , alloc_obj :: !Int {--Number of objects allocated --}
				   , promotions :: !Int {-- How many objects get promoted? --}
				   , chunks :: !(Seq.Seq S.IntSet)
				   , marks :: !Int --Just a count of marking done by the GC
				   , nursery_collections :: !Int
				   , whole_heap_collections :: !Int
				   , large :: !Int --Objects bigger than this size are considered "large"
				   , large_objects :: !S.IntSet	
				   , time :: !Int --What time is it now?
				   , objMap :: !(M.IntMap Size)
				   , origin2chunkIndex :: !(M.IntMap Int)
				 } deriving (Show,Eq)


initMachine = ReachMachine {
                               nursery = S.empty
                             , nursery_size = 0 
                             , nursery_limit = 4*mb
                             , rem_set = MS.empty
			     , rem_set_origins = MS.empty
                             , heap_size = 0 
                             , heap_limit = 2*gb 
                             , heap = G.empty
                             , roots = MS.empty
                             , stacks = M.empty
                             , alloc_bytes = 0
			     , alloc_obj = 0
			     , promotions = 0
                             , chunks = Seq.empty
                             , marks = 0
			     , nursery_collections = 0 
			     , whole_heap_collections = 0
                             , large = 2 * mb
			     , large_objects = S.empty
			     , time = 0
			     , objMap = M.empty
			     , origin2chunkIndex = M.empty
                       }

instance NFData ReachMachine where
	rnf m =            (nursery m) 
		`deepseq`  (nursery_size m)
                `deepseq`  (nursery_limit m)
		`deepseq`  (rem_set m)
		`deepseq`  (rem_set_origins m)
		`deepseq`  (heap_size m)
		`deepseq`  (heap_limit m)
		`deepseq`  (heap m)
		`deepseq`  (roots m)
		`deepseq`  (stacks m)
		`deepseq`  (alloc_bytes m)
		`deepseq`  (alloc_obj m)
		`deepseq`  (promotions m)
		`deepseq`  (chunks m)
		`deepseq`  (marks m)
		`deepseq`  (nursery_collections m)
		`deepseq`  (whole_heap_collections m)
		`deepseq`  (large m)
		`deepseq`  (large_objects m)
		`deepseq`  (time m)
		`deepseq`  (objMap m)
		`deepseq`  (origin2chunkIndex m)
		`seq`      m
		`seq`	   ()



heapSize::ReachMachine->Int
heapSize m = S.foldl' (\sum n-> ((objMap m) ! n) + sum)  0 (nodes $! heap m)

--mergeChunks i j m
--produces a machine with chunks i,j merged
--origin2chunkIndex map is updated accordingly
mergeChunks::Int->Int->ReachMachine->ReachMachine
mergeChunks i j m = if i >= (Seq.length $ chunks m) || j >= (Seq.length $ chunks m) then
		      error "Index out of bound for chunnks."
		    else
		      m{chunks=chunks',origin2chunkIndex=o2c'}
  where
	!mergedChunk = (chunks m) `seq` ((chunks m) `index` i) `S.union` ((chunks m) `index` j)


	o2c' = M.map f $! (origin2chunkIndex m)
	  where
	    f idx 
	       |  idx == i || idx == j           =  0
	       |     idx < (min i j)             = idx + 1
	       |  idx > min i j && idx < max i j = idx
	       |  idx > max i j                  = idx -1 

        chunks' = mergedChunk <| (deleteAtIndex (S.fromList [i, j]) $! (chunks m))

	deleteAtIndex::S.IntSet->Seq.Seq a ->Seq.Seq a
 	deleteAtIndex is as = delSub is 0 as
	  where
	   delSub::S.IntSet->Int->Seq.Seq a->Seq.Seq a
	   delSub is  cur as  
		           |  Seq.null as     =  if S.null is then
				     		   Seq.empty --We got here after consuming all of is
				   		 else
				     		   --Should never get here!
				                   error ("Index " ++ show is ++ "too big for list")
			   |    otherwise     =   if  cur `S.member` is then
				     		    if S.null is then
					  	      as
				     		    else
					  	      delSub  (S.delete cur is) (cur+1) (Seq.drop 1 as)
				                  else
						       let !recCall = (delSub is (cur+1) (Seq.drop 1 as)) in
				                       (as `index` 0) <| recCall



repOk::String->ReachMachine->ReachMachine
repOk s m  
	| not  (all membPred $ S.toList $ G.nodes $ heap m) = 
		error ( s ++ (show $ fromJust $ find  (not.membPred) $  S.toList $ G.nodes $heap m ) 
			  ++ " is in the heap, but not in any chunk,  the nursery, or the large object space.")
        |(MS.toSet $ rem_set_origins m) `S.intersection` (nursery m) /= S.empty = 
		 error (s ++ "Error, rem_set origins and the nursery are not disjoint.")
	| 0 `S.member` nursery m  						= error (s ++ "0 in nursery")
	| 0 `MS.member` rem_set_origins m 					=  error (s ++ "0 is in rem_set_origins")
        | 0 `MS.member` rem_set m 					        = error (s ++ "0 is in rem_set")
	| 0 `S.member` (nodes $ heap m)						= error (s ++ "0 is in the heap")
	| not  chunkIndexTest							= error ((show m) ++ "origin2chunkIndex does not align with actual contents of chunk.")
	| otherwise 								= m
	where
	   allChunks = F.foldl' S.union S.empty (chunks m)
	   membPred n =  n `S.member` allChunks || n `S.member` (nursery m) || n `S.member` (large_objects m)--}
	   chunkIndexTest = all (\r -> r `S.member` ((chunks m) `index` ( (origin2chunkIndex m) ! r))) $  (M.keys $  origin2chunkIndex m)


addToHeap::Int->ReachMachine->ReachMachine
addToHeap oid m = m{heap= G.insNode oid (heap m)}


instance Machine ReachMachine where
  --allocate::ObjectID->Size->String->ThreadID->ReachMachine->ReachMachine
  allocate  !oid !size unused1 unused2 !m = if size > large m then
					    {-# SCC "largeObjAlloc" #-} largeObjAlloc oid size
					  else
				    	    {-# SCC "generalAlloc" #-} generalAlloc oid size 
    where
      largeObjAlloc oid size = addToHeap oid $ m{large_objects= ( oid `S.insert` large_objects m), objMap=objMap'}
	  where
	    objMap' = M.insert oid size $! objMap m
			       
      generalAlloc oid size 
		  | (doCollection && doNurseryCollection) = allocate oid size unused1 unused2 (wCollectedMachine{objMap=objMap',chunks=chunks', heap_size=heapSize wCollectedMachine})  --need to try allocating again
		  | doNurseryCollection = allocate oid size unused1 unused2 nCollectedMachine{objMap=objMap'}  
		  | otherwise = allocedMachine{objMap=objMap'}
	where
	  nursery' = S.insert oid $!  (nursery m)
	  heap_size' = (heap_size m + size)
	  heap'  =  G.insNode oid (heap m)
	  doCollection = heap_size' > heap_limit m
	  doNurseryCollection = nursery_size' > nursery_limit m
	  nursery_size' = (nursery_size m) + size
	  alloc_bytes' = (alloc_bytes m) + size
	  alloc_obj' = (alloc_obj m) + 1

	  nCollectedMachine =  (reachCollect m){alloc_bytes=alloc_bytes', alloc_obj=alloc_obj', nursery_collections = nursery_collections m + 1, nursery=(S.singleton oid), nursery_size=0}

	  allocedMachine =   m{nursery=nursery', nursery_size = nursery_size', heap_size=heap_size', heap=heap', alloc_bytes =alloc_bytes', alloc_obj=alloc_obj'}

	  {--TODO: This shouldn't zero the size; it should fiugre out teh size based on live obejcts...--}
	  wCollectedMachine = (GS.wholeHeapCollect nCollectedMachine){whole_heap_collections=whole_heap_collections nCollectedMachine + 1, heap_size=0}

	  chunks' =  if doNurseryCollection && doCollection then
		       purgeChunks (\n->G.hasNode n $!  heap wCollectedMachine)  $! chunks wCollectedMachine
		     else
		       chunks wCollectedMachine

	  objMap' = M.insert oid size $! objMap m
							  

  --death::ObjectID->ReachMachine->ReachMachine
  death !oid !m = m
     {- {-# SCC "death" #-} m{nursery=nursery', heap=heap', rem_set=rem_set', rem_set_origins=rem_set_origins', large_objects=large_objects',objMap=objMap'} 
     where
       nursery' = S.delete oid (nursery m)
       heap'    = G.delNode oid (heap m)
       rem_set' = MS.deleteAll oid (rem_set m)
       rem_set_origins' = MS.deleteAll oid (rem_set_origins m)
       large_objects' = S.delete oid (large_objects m)
       hasObj = M.member oid (objMap m)
       --objMap' =M.adjust (\(ObjectModel aTime _ size chunk)->ObjectModel aTime (Just $ time m) size chunk) oid $ objMap m
       objMap' = M.delete oid (objMap m)
     -}

  --update::ObjectID->ObjectID->ObjectID->Maybe ObjectID->ObjectID->ReachMachine->ReachMachine		     
  update !old !origin !new _ _ !m = {-# SCC "update" #-}  
					  let 
					   m' =  updateChunks $! updateO2C $! updateRemSet $! updateRoots $! updateHeap $! updateNodes m  --m'{heap=heap', rem_set=rem_set'', rem_set_origins=rem_set_origins'',roots=roots', origin2chunkIndex=origin2chunkIndex'}
					  in
					   m'
						    
				     where
					  updateHeap !m = {-# SCC "updateHeap" #-} m{heap=heap'}
							 where
							   !heap' = updateEdges m --G.insEdge origin new $! G.delEdge origin old (heap m')
	  


					  updateRoots !m = {-# SCC "updateRoots" #-}m{roots=roots'}
							  where
							    !roots'
								| origin == 0 && new /= 0 =  MS.insert new $! MS.delete old $! (roots m)
							        | origin == 0 && new == 0 =  MS.delete old $! (roots m)									       
								| otherwise               = roots m
					  
					  {--Check of the new pointer is a adulut->nursery pointer, update rem set accordingly--}	
					  updateRemSet !m = {-# SCC "updateRemSet" #-} m{rem_set=rem_set'',rem_set_origins=rem_set_origins''}
							   where
							     !rem_set' = if (not $ origin `S.member` nursery m) && new `S.member` nursery m then
									  MS.insert new $! (rem_set m)
									else
									  rem_set m

									       {--Check to see if the old target was in the remembered set and remove it. --}
							     !rem_set'' = if (not $ origin `S.member` nursery m) && old `S.member` nursery m then
									      MS.delete old rem_set' {-- Removes only one copy; rem_set' is a MultISet--}
									 else
									      rem_set'
							
							     !rem_set_origins' = if ( (origin /= 0) &&( not $ origin `S.member` nursery m )) && (new `S.member` nursery m ) then
										  MS.insert origin $!  (rem_set_origins m)	
										else
										  rem_set_origins m
							      
							     !rem_set_origins'' = if ( (origin /= 0) && ( not $ origin `S.member` nursery m ) ) && old `S.member` nursery m  then
										    MS.delete origin rem_set_origins'
										 else
										    rem_set_origins'


					  updateO2C !m = {-# SCC "updateO2C" #-} m{origin2chunkIndex=origin2chunkIndex'}
							where
							  !origin2chunkIndex' = if ( (origin /= 0) 
										   &&( not $ origin `S.member` nursery m )) 
										   && (new `S.member` nursery m ) 
										   && not  (origin `S.member`(large_objects m)) then
										   M.insert origin index $! (origin2chunkIndex m)
									       else
										   origin2chunkIndex m
						  
									       where
										  index = findChunkIndexWith  m origin
					  
					  updateChunks !m = {-# SCC "updateChunks" #-}
							   if notInNursery origin && notInNursery new && (originIndex /= newIndex) then
							      mergeChunks originIndex newIndex m
							   else
							       m
							      
							   where
							     notInNursery i = (not (i `S.member` (nursery m))) && i /= 0 && not (i `S.member` (large_objects m))
							     originIndex = findChunkIndexWith m origin
							     newIndex    = findChunkIndexWith m new	

					  findChunkIndexWith::ReachMachine->Int->Int
					  findChunkIndexWith m o = {-# SCC "findChunkIndexWith" #-}
								   fromMaybe f $ Seq.findIndexL (\c ->o `S.member` c) $! (chunks m)
								   where
								     f = error $  ("Find hcunkIndex died on " ++ (show o) ++ "o in nursery? " ++ (show $ o `S.member` nursery m)
										   ++ "O in large objects? "++ (show $ o `S.member` large_objects m))

					  test m1 m2 = chunkUnion m2 `S.isSubsetOf` chunkUnion m1 
						       where
							 chunkUnion m = F.foldl' S.union S.empty (chunks m)
									  

					  {-- It is a little weird, but some things may be missing allocations, so this is the first we hear of them --}
					  updateNodes m = {-# SCC "updateNodes" #-} foldl' (\mach n -> allocate  n 0 L.empty 0 mach  ) m $ filter (addNode) [origin, new, old]
					  addNode n = {-# SCC "addNode" #-}n /= 0 && not (G.hasNode n  $ heap m) --Not zero, and not in the heap
				  
					  updateEdges m 
						  | (origin == 0)          = heap m
						  | (old /= 0 && new /= 0) = G.insEdge origin new $! G.delEdge origin old (heap m)
						  | old == 0               = G.insEdge origin new (heap m)
						  | new == 0		   = G.delEdge origin old (heap m)
								  

							   
                                                    
  --methodEntry::MethodID->ObjectID->ThreadID->ReachMachine->ReachMachine
  methodEntry  !mid _ !tid !m = {-# SCC "entry" #-} m{stacks=stacks', time=(time m + 1)}
				where
				  stacks'=push mid tid (stacks m)

  --methodExit::MethodID->ObjectID->ThreadID->ReachMachine->ReachMachine
  methodExit  !mid _ !tid !m = {-# SCC "exit" #-} m{stacks=stacks', time = (time m + 1)}
			     where
		              !stacks' = pop mid tid (stacks m)


   --root::ObjectID->ThreadID->ReachMachine->ReachMachine
  root !r !t !m = {-# SCC "root" #-} m{stacks=GS.addRoot r t $! (stacks m)} 

instance CollectableMachine ReachMachine where
  stacks = ReachGC.stacks
  heap   = ReachGC.heap
  roots  = ReachGC.roots
  replaceHeap h m = m{heap=h}
  incMarks i   m  = m{marks=(marks m) + i}

--Strict left fold over sets-
--S.foldl'::(Int->b->b)->b->S.IntSet->b
--S.foldl' f b s =  List.foldl' (\b i->f i b)  b (S.toList s)


{--TODO: this is ugly and inefficeint. Fix one, preferably both, of those things.--}
reachCollect::ReachMachine->ReachMachine
reachCollect !m =
   let m' = trace (("Doing reach collect: heap_size: " ++ show  (heap_size m)) ++ "chunks: " ++ show (Seq.length $ chunks m) ++ "origin2chunkIndex: " ++ show (M.size $ origin2chunkIndex m) ) $
	    m{heap=sweepedHeap
					  , objMap=sweepedObjMap
					  , marks=(marks m + S.size marked)
					  , chunks=chunks'
                                          , nursery=S.empty
					  , nursery_size=0
					  , rem_set = MS.empty
					  , rem_set_origins = MS.empty
					  ,heap_size=(heap_size m) - size_of_deleted  
					  ,origin2chunkIndex = M.empty
					  } 
	    in m' --`deepseq` m'
   where
      -- nurserySearch roots heap = mark S.empty roots 
   
       marked = {-# SCC "marked" #-} mark S.empty (MS.toSet (roots m) `S.union` MS.toSet (rem_set m) `S.union` GS.getRoots m `S.union` MS.toSet (rem_set_origins m))
       --Remove those things from the heap that are in the nursery, and unmarked
       sweepedHeap = S.foldl'  (flip G.delNode) (heap m) $ S.filter (\x -> not $  S.member x marked) $! nursery m

       --We don't need to keep anyhting that is not marked
       sweepedObjMap = S.foldl' (flip M.delete) (objMap m) $ S.filter (\x -> not $  S.member x marked) $! nursery m

       --fold over rem_set,root_set, building map root->nursery objects reachable from that root
       root2BabiesMap::M.IntMap S.IntSet
       {--root2BabiesMap = {-# SCC "root2BabiesMap" #-}    M.fromList $  map  (\r-> (r, followRoot r))  (S.toList $  MS.toSet (rem_set_origins m))--}
       (!root2BabiesMap,!orphans) = {-# SCC "root2BabiesOrphans" #-}S.foldl' evictBabies (M.empty,(nursery m)) parents
       				    where
				      parents = (S.filter ( \n -> G.hasNode n  sweepedHeap) $ MS.toSet $! rem_set_origins m)
				      --Give a root r, a map of roots to babies r, and a set of babies,
                                      -- add to the map everything in babies reachable from r, and remove them from babies
				      --  babies' == babies - (every thing in the nursery reachable from r)
	 			      evictBabies (map,babies) r  = (M.insert r rbabies map, babies')
			  	       where
					 --the babies of r
	  	 		         rbabies = S.filter (\n-> n `S.member` babies ) $ followRoot r
             			         babies' = babies S.\\ rbabies	

       size_of_deleted = S.foldl' (\sum o ->  ((objMap m) ! o) + sum ) 0 $ S.filter (\n -> not $ S.member n marked) (nursery m)

       chunkIndex2Babies::M.IntMap S.IntSet
       chunkIndex2Babies = foldl' (\map r -> M.insertWith (S.union) (ci r) (babies r) map ) M.empty $ M.keys root2BabiesMap
			    where
			      ci r = (origin2chunkIndex m) ! r
			      babies r = root2BabiesMap ! r

              --inAnyChunk id = any  (S.member id) chunks'

       


       {--The nursery objects that are alive and  have no parent in the adult generation --}
       !liveOrphans =  S.filter (\n-> n `S.member` marked) orphans
	
       {--All the orphans get their own chunk--}
       !chunks' = if not $ S.null liveOrphans then --Don't append liveOrphans if it is empty
			(promoteIntoChunks (chunks m)) |> liveOrphans --Putting the new chunk at the end means we don't have to update origin2chunkIndex maps
		  else
			promoteIntoChunks (chunks m)


       promoteIntoChunks cs = {-# SCC "goOverChunks"  #-} promoteIntoChunksSub cs 0
			 where
			   promoteIntoChunksSub  cs i 
						| Seq.null cs    = Seq.empty
						|    otherwise   = (head cs `S.union` (M.findWithDefault S.empty i chunkIndex2Babies)) <| promoteIntoChunksSub (tail cs) (i+1)      

						where
						    head::Seq.Seq a -> a
						    head s = s `index` 0
						
                                                    tail s = Seq.drop 1 s
						

			   --goOverChunksSub (c:cs) i =  (c `S.union` (M.findWithDefault S.empty i chunkIndex2Babies)):(goOverChunksSub cs (i+1))
			   --goOverChunksSub   []   _ =  []


 
       {-- A set of all nodes in the nursery reachable by starting at r, and only following pointers
	   that point within the nursery. This set does not include r. --}
       followRoot::Int->S.IntSet
       followRoot !r = S.delete r $ mark S.empty (S.singleton r) 

       mark::S.IntSet->S.IntSet->S.IntSet
       mark black grey = if S.null grey  then
			   black
			 else
			   mark black' grey'
	where
	  black'::S.IntSet
	  !black'  = S.foldl'  (flip S.insert) black grey
	  grey' = S.foldl' (flip $ S.union.removeBlack.nurseryNeighbors) S.empty black'
	 
	  removeBlack  = S.filter (\x -> not $ S.member x black)

	  nurseryNeighbors::Int->S.IntSet
	  nurseryNeighbors n = S.fromList $ filter (\x->S.member x (nursery m)) $  fromMaybe [] $ G.neighbors n (heap m)

       
       tests = markedInSomeChunkTest && babyAgreementTest 
	      where
		chunkUnion = F.foldl' S.union S.empty chunks' 

	        babyUnion = (M.fold S.union S.empty root2BabiesMap) `S.union` liveOrphans

		{--Everything makred should end up in some chunk, so if makred - (union of all chunks) isn't the empty set, something has gone wrong--}
	        markedInSomeChunkTest = if S.null (marked \\ chunkUnion) then
			       	      True
 		     	            else
				      error $  "Some how something makred did not end up in chunkUnion " ++ show (marked \\ chunkUnion) ++ " " ++ (show $ S.null liveOrphans) 
					++ (show $  9647 `S.member` babyUnion) ++ (show $ 9647 `S.member`orphans)
		
		ciBabyUnion = M.fold S.union S.empty chunkIndex2Babies `S.union` liveOrphans
		{--Although they are indexed in different ways, the union of these sets of babies should be the same --}
	        babyAgreementTest = (ciBabyUnion == babyUnion) 
  




{--For all n, if keepFunc n is false, the resulting chunks will not contain n--}
purgeChunks::(Int->Bool)->Seq.Seq S.IntSet->Seq.Seq S.IntSet
purgeChunks keepFunc chunks = fmap (S.filter keepFunc) chunks



			      
--Used after a collection;
--purge anything from any part of the data structure that is not in the eap
purgeObjectMap::ReachMachine->ReachMachine
purgeObjectMap m = m{objMap=M.filterWithKey  (\k v ->  k `S.member` (nodes $ heap m))  (objMap m)}

