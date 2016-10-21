import Data.IntMap as M
import EtParser
import System (getArgs)
import Data.List as List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Graph.Inductive as G
import Data.IntSet as S
import Data.Graph.Inductive.Query.DFS as GQ


simStep::M.IntMap (M.IntMap Int) -> Record -> M.IntMap (M.IntMap Int)


simStep m (Update old origin new field thread) = if origin `M.member` m then
						M.update updateAdjList origin m
					   else
						if old == 0 then
							M.insert origin (M.fromList [(new,1)]) m
						else
							m
                                           where
                                                 incCount::Int->Maybe Int
                                                 incCount c = if old == 0 then
								       if c >= 0 then --When things reach 0, they are stuck
        	                                                          Just 0
                  	                                               else
                          	                                           Just $! c+1
							       else
									Nothing

                                                 decCount c = Just $ c - 1
		
                                                 --f takes the map for object 'origin' and updates the counters
                                                 updateAdjList::IntMap Int->Maybe (IntMap Int)
                                                 updateAdjList m = if new `M.member` m then
							  		Just $! M.update incCount new (M.update decCount old m)
	             						   else
							  		Just $! M.insert new 1 (M.update decCount old m)

                                                 --Values in the adjacency matrix <0 indicate unstable pointers;
					  --Since the object is dead, we can get rid of them.
simStep m (Death oid)			= M.update (Just.(M.filter (< 0))) oid m
simStep m    _				= m



setOfDests::M.IntMap (M.IntMap Int)->S.IntSet
setOfDests m = List.foldl' S.union S.empty  $! List.map ( \(_,m')-> toDestSet $ assocs m' )    (assocs m) 
		where
			toDestSet::[(Int,Int)]->IntSet
			toDestSet is = S.fromList $! List.map (\(d, _ ) -> d) is


insNodeIfNeeded::Int->ObjGraph->ObjGraph
insNodeIfNeeded n g = if G.gelem n g then
			g
		      else
			G.insNode (n,n) g
	


type ObjGraph = G.Gr Int Int


--Convert from the Map Node -> (Map Node->Count) ad hoc representation to a FGL Graph 
convert::M.IntMap (M.IntMap Int) -> ObjGraph
convert m  = foldl' (\ !g !k-> insEdges  g k (fromJust $ M.lookup k m)  ) ng (M.keys m)
	     where
	       --The "Node Graph", has all the nodes, no edges
	       !ng_keys = foldl'  (\ !g !n-> G.insNode (n,n) g) G.empty (M.keys m)
	       --Some are only seen as destinations in our original representation,
	       --not as origins, we find them this way
	       !ng      = S.fold (\ !n !g -> insNodeIfNeeded n g) ng_keys (setOfDests m) 

	       insEdges::ObjGraph->Int->IntMap Int->ObjGraph
	       insEdges !g !origin !dests = M.foldWithKey (\dest count graph -> insDest origin graph dest count) g dests

	       insDest::Int->ObjGraph->Int->Int->ObjGraph
	       insDest  !origin !g !d !count = if count /= 0 then
	   						G.insEdge (origin, d, count) g
					       else
							g
			

simulate::[Record]->M.IntMap (M.IntMap Int)
simulate rs = List.foldl' simStep M.empty rs


main = do
	args <- getArgs
	contents <- L.readFile (args !! 0)

	print $! GQ.noComponents $! convert $! simulate $! List.map (fst.fromJust.readRecord) $! L.lines contents
