import EtParser

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.Maybe
import System (getArgs)



data Machine = Machine {
			 heap :: !G.Graph
			 time :: !Int
			 time2dead :: M.IntMap  S.IntSet --Maps a time to a number of dead objects
		       }
		       deriving (Eq, Show)



simStep::(Machine,Int)->Record->(Machine,Int)
simStep (m,!marks) (Alloc oid size ty tid) = if doCollection then
				       	 	(collectedMachine{alloc=alloc'}, marks+newMarks)
					     else
					        (m{nursery=nursery', heap_size=heap_size', heap=heap', alloc=alloc'},marks)
					     where
						nursery' = S.insert oid (nursery m)
						heap_size' = (heap_size m + size)
						heap'  =  G.insNode oid (heap m)
						doCollection = heap_size' > heap_limit m
						(!collectedMachine,!newMarks) = wholeHeapCollect m
						alloc' = (alloc m) + size

simStep (m,!marks) (Update old origin new tid) = (m{heap=heap'}, marks)
				   where
					!heap' = G.insEdge origin new $! G.delEdge origin old (heap m)

simStep (m,!marks) (Entry mid rid tid) = (m{time= (time m) + 1},marks)
					 where
						if 

simStep (m,!marks) (Exit mid rid tid) = (m{time= (time m) +1},marks)







loadDeathMap::L.ByteString -> Maybe (M.IntMap S.IntSet)
loadDeathMap s = Just $ M.fromList $  map (fromJust) $ filter isJust $ map readOneRecord $  L.lines s

readOneRecord::L.ByteString->Maybe (Int,S.IntSet)
readOneRecord s = do
		(time, s') <- readHex $ trim s
		(colon,s'') <- readString $ trim s'
		oids <- readOids (trim s'') []
	     
		return (time, S.fromList oids)
	     

readOids::L.ByteString->[Int]->Maybe [Int]
readOids s oids =  case res of
		 Just (id,rest) -> if L.null rest  then
				    Just (id:oids)
				  else 
				    readOids (trim rest) (id:oids)
		 Nothing        -> Nothing
	       where
		  res = readHex s		

		    

main = do
	 args <- getArgs
	 contents <- L.readFile (args !! 0)
	
	 print contents
	 print $! loadDeathMap contents

