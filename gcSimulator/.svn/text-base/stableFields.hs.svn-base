import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Data.Maybe as Maybe
import Data.List as List
import System (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import EtParser


nursery_size = 4194304 --in bytes
data Machine = Machine {
			   objMap:: !(M.IntMap (M.IntMap Bool))
			 , metaUnstable :: !S.IntSet
			 , metaStable ::   !S.IntSet
			 , numFields :: !Int
			 , stabFields:: !Int
			 , numAdultFields:: !Int
			 , stabAdultFields :: !Int
			 , allocMap:: !(M.IntMap Int)
			 , time:: !Int
		       } deriving Show

initMachine::Machine
initMachine =  Machine{objMap=M.empty, metaUnstable=S.empty, metaStable=S.empty, numFields=0, stabFields=0, numAdultFields=0, stabAdultFields=0, allocMap=M.empty, time=0}

--simStep::(M.IntMap (M.IntMap Bool), Int, Int)->Record->(M.IntMap (M.IntMap Bool), Int, Int)
simStep::Machine->Record->Machine

simStep mach (Update _ origin _ field _) = mach{objMap=objMap',metaStable=metaStable', metaUnstable=metaUnstable'}
	where
		!objMap' = if M.member origin (objMap mach) then
				let !m = Maybe.fromJust $ M.lookup origin (objMap mach)
			   	    !m' = if M.member field (objMap mach) then
	  		    	    		M.insert field False m
		        		 else
				  		M.insert field True m
				in
					M.insert origin m' (objMap mach)
			  else
				let !m = M.fromList [(field, True)]
				in
				   M.insert origin m (objMap mach)

		
		{--!metaFieldMap' = if M.member field (metaFieldMap mach) then
				    M.insert field ((fromJust $ M.lookup field (metaFieldMap mach)) && (fromJust $ M.lookup field $ fromJust $ M.lookup origin objMap')) (metaFieldMap mach)
	       		         else
				    M.insert field True (metaFieldMap mach)--}

		metaStable' = if S.member field (metaStable mach) && (not $ S.member field (metaUnstable mach)) then
					S.delete field (metaStable mach)
				    else
					S.insert field (metaStable mach)
		
		metaUnstable' = if S.member field (metaStable mach) then --Now it is unstable
					 S.insert field (metaUnstable mach)
				      else
					 (metaUnstable mach)
				

			
				
simStep mach (Alloc oid size _ _) = mach{allocMap=allocMap', time=time'}
	where
		time' = (time mach) + size
		allocMap' = M.insert oid (time mach) (allocMap mach)
	


simStep mach (Death oid) = mach{objMap=objMap', numFields=numFields', stabFields=stabFields', numAdultFields=numAdultFields', stabAdultFields=stabAdultFields', allocMap=allocMap'}
	where
		!objMap'     = M.delete oid (objMap mach)
		!allocMap'   = M.delete oid (allocMap mach)
		!numFields'  = (numFields mach) + fieldsInObj
		!stabFields' = (stabFields mach) +  stabFieldsInObj

		fieldsInObj = countFields $ M.lookup oid (objMap mach)
		stabFieldsInObj = countStabFields $ M.lookup oid (objMap mach)

		!numAdultFields' = if isAdult then
					(numAdultFields mach) +  fieldsInObj
				  else
					(numAdultFields mach)

		!stabAdultFields' = if isAdult then
					(stabAdultFields mach) + stabFieldsInObj 
				     else
					(stabAdultFields mach)

		!isAdult  = if oid `M.member` allocMap mach then
				(time mach) - (fromJust $ M.lookup oid (allocMap mach)) > nursery_size
			   else
				False

	

simStep tuple _ = tuple


countFields::Maybe (M.IntMap Bool)->Int
countFields Nothing = 0
countFields (Just fieldMap) = length $ M.keys fieldMap

countStabFields::Maybe (M.IntMap Bool)->Int
countStabFields Nothing         = 0
countStabFields (Just fieldMap) = length $ M.keys $ M.filter id fieldMap




simulate::[Record]->Machine
simulate rs = endMach{stabFields= (stabFields endMach )+ endStabCount, numFields=(numFields endMach) + endFieldCount}
	where 
		!endMach = List.foldl' simStep initMachine rs
		!endStabCount  = M.fold  (+)   0  $! M.map (\x-> countStabFields (Just x)) (objMap endMach)
		!endFieldCount = M.fold  (+)   0  $! M.map (\x-> countFields (Just x)) (objMap endMach)


main = do
	args <- getArgs
	contents <- L.readFile (args !! 0)
	let endMach =  simulate $! map (f.readRecord) $! L.lines  contents

	
	print "Meta Stable Fields: "
	mapM  print (S.toList $ metaStable endMach) 

	
	where
	      f::Maybe (Record,L.ByteString) -> Record
	      f  (Just (r,_))   = r
	      f      _        = error "We are all going to die."
	      
