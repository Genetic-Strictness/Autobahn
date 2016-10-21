module ObjectToTypeMap where
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as M
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map ((!))
import Data.Maybe 

type ObjectID = Int

data ObjectToTypeMap = ObjectToTypeMap {
			    obj2tid :: !(M.IntMap ObjectID)
			  , tid2type :: !(M.IntMap L.ByteString)
			  , type2tid :: !(Map.Map L.ByteString ObjectID)
			  , last_tid :: !Int
			} deriving (Eq, Show)

empty = ObjectToTypeMap M.empty M.empty Map.empty 0


insert::ObjectID->L.ByteString->ObjectToTypeMap->ObjectToTypeMap
insert  oid ty !o2t =  if ty `Map.member` (type2tid o2t) then
			let
			  obj2tid' = M.insert oid ((type2tid o2t) ! ty ) (obj2tid o2t)
			in
			  o2t{obj2tid=obj2tid'}
		      else
			let
			   last_tid' = (last_tid o2t) + 1
			   type2tid' = Map.insert ty last_tid' (type2tid o2t)
			   tid2type' = M.insert last_tid' ty (tid2type o2t)
			   obj2tid' = M.insert oid last_tid' (obj2tid o2t)
			in
			  ObjectToTypeMap obj2tid' tid2type' type2tid' last_tid'
			
		

lookup::ObjectID->ObjectToTypeMap->Maybe L.ByteString
lookup oid (ObjectToTypeMap !o2tid !tid2ty _ _)  = do
						    tid <- M.lookup oid o2tid
						    M.lookup tid tid2ty			


toList::ObjectToTypeMap->[(ObjectID,L.ByteString)]
toList !o2t =  List.foldl' (\l oid -> (oid, fromJust $ ObjectToTypeMap.lookup oid o2t):l) [] $  M.keys (obj2tid o2t)
