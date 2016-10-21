import EtParser
import qualified Data.IntMap as M
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Query.DFS as GQ
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as S
import Data.Maybe
import Data.List as List
import System (getArgs)

data Machine = Machine {
			  graph :: !(G.Gr Int Int)
			, stableFields :: !S.IntSet 
		       } deriving Show


simStep::Machine->Record->Machine
simStep m (Update old origin new field thread)=
	 if field `S.member` (stableFields m)  then 
		m{graph=graph'}
	 else
		m
	 where
		graph' = G.insEdge (origin, new, field) $ G.delEdge (origin, old)  (graph m)
simStep m _ = m



emptyMachine = Machine{graph=G.empty,stableFields=S.empty}

simulate::S.IntSet->[Record]->Machine
simulate fields rs = List.foldl' simStep emptyMachine{stableFields=fields} rs

main = do
	args <- getArgs
	fields <- L.readFile (args !! 0)
	recs <- L.readFile (args !! 1)

	let stabFieldSet = foldl (\fs f-> S.insert f fs) S.empty  $! map (fst.fromJust) $! filter isJust $! map L.readInt $!  L.lines fields
	print $! simulate stabFieldSet $! map (fst.fromJust.readRecord) $! L.lines recs 	
