import NurseryGC
import qualified Data.IntSet as S
import qualified Data.IntMultiSet as MS
import qualified Graph as G
import Machine

nurse = S.fromList [1,2]

node_heap = foldl (\g n -> G.insNode n g) G.empty [1,2,3]

hp = G.insEdge 3 1 node_heap

rs = MS.singleton 1
rso = MS.singleton 3

test_m = initMachine{nursery=nurse, heap = hp, rem_set=rs, rem_set_origins=rso}


res_m = nurseryCollect test_m


test_heap = hasNode 1 && hasNode 3 && (not $ hasNode 2)
	     where
		 hasNode = \n -> G.hasNode n (heap res_m)

test_nursery_empty = (nursery res_m) == S.empty


passFail::String->Bool->String
passFail s b =  if b then
		    s ++ "PASSED"
		else
		    s ++ "FAILED"


main = do
	  putStrLn $ passFail "Nodes correct: " test_heap
	  putStrLn $ passFail "Nursery Empty: " test_nursery_empty
	  putStrLn $ show res_m
	 
