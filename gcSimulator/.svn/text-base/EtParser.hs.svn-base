module EtParser where


import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.IntSet as S
import qualified Data.Char as C
--import qualified Graph as G
import qualified Data.Bits as Bits
--import Data.List as List
import Machine



data Record =  Alloc {-# UNPACK #-} !Int !Int L.ByteString !Int
	     | Update !Int !Int !Int !(Maybe Int) !Int
	     | Death !Int
	     | Entry !Int !Int !Int
	     | Exit  !Int !Int !Int
	     | Root  !Int !Int
	     deriving (Eq, Show)




readHex::L.ByteString -> Maybe (Int, L.ByteString)
readHex s  = if L.null hex_string then
		Nothing
	     else
		Just (hatoi hex_string, rest)
          where
 	        hex_string = L.takeWhile C.isHexDigit s
        	rest	   = L.dropWhile C.isHexDigit s
		hatoi::L.ByteString->Int
		hatoi s = L.foldl'  (\i c -> (Bits.shiftL i 4) + C.digitToInt c) 0 s


readHexFast::L.ByteString -> (Int, L.ByteString)
readHexFast s = if L.null hex_string then
	      error ("Could not parse hex string")
	    else
	      (hatoi hex_string, rest)
            where
 	      hex_string = L.takeWhile C.isHexDigit s
              rest	 = L.dropWhile C.isHexDigit s
	      hatoi::L.ByteString->Int
	      hatoi s = L.foldl'  (\i c -> (Bits.shiftL i 4) + C.digitToInt c) 0 s

--trim leading white space 
trim::L.ByteString -> L.ByteString
trim  = L.dropWhile C.isSpace 


readString::L.ByteString -> Maybe (L.ByteString,L.ByteString)
readString s = Just ( L.copy $ L.takeWhile (notSpace) s , L.dropWhile (notSpace) s)
		where
			notSpace = not.C.isSpace

readStringFast::L.ByteString -> (L.ByteString,L.ByteString)
readStringFast s = ( L.copy $ L.takeWhile (notSpace) s , L.dropWhile (notSpace) s)
		where
			notSpace = not.C.isSpace



readRecord::L.ByteString -> Maybe (Record, L.ByteString)
readRecord s

	| firstChar == 'R' = do
				(rid,rest') <- readHex $ trim rest
				(tid, rest'') <- readHex $ trim rest'

				return (Root rid tid, rest'')
	| firstChar == 'M' = do
				(mid,rest') <- readHex $ trim rest
				(rid,rest'') <- readHex $ trim rest'
				(tid,rest''') <- readHex $ trim rest''	

				return (Entry mid rid tid, rest''')
	| firstChar == 'E' = do
				(mid, rest') <- readHex $ trim rest
				(rid, rest'') <- readHex $ trim rest'
				(tid, rest''') <- readHex $ trim rest''
	
				return (Exit mid rid tid, rest''')

	| firstChar == 'U' = do
				{--This is complicated because Updates can have am optional field id.
				   So they can look like:
					 U old origin new fieldID threadID 
				   or:   
					 U old origin new threadID
				   We handle both cases with a Maybe
                                --}
					
				(old, rest') <- readHex $ trim rest
				(origin, rest'') <- readHex $ trim rest'
				(new, rest''') <- readHex $ trim rest''
				(fidOrTid,rest'''') <- readHex $ trim rest'''
				--(tid, rest''''') <- readHex $ trim rest''''
				let maybeTid = readHex $ trim rest''''
				
				case maybeTid of 
					Just (tid,rest5) -> return  (Update old origin new (Just fidOrTid) tid, rest5)
					Nothing          -> return  (Update old origin new Nothing fidOrTid, rest'''')

	| firstChar == 'A' =  do
				(oid,rest') <- readHex $ trim  rest
				(size,rest'') <- readHex $   trim rest'
				(ty,rest''') <- readString $! trim rest''
				(tid, rest'''') <- readHex $ trim rest'''
				
				return (Alloc oid size ty tid, rest'''')
	| firstChar == 'D' = do
				(oid,rest') <- readHex $ trim rest
				
				return (Death oid, rest')

	

	| otherwise        = error $ "EtParser: Bad starting character" ++ [firstChar]
	where
		firstChar = L.head s
		rest =	L.tail s


readRecordMachine::Machine m=>m->L.ByteString->m
readRecordMachine  m s
	| firstChar == 'A' =  do
				let (oid,rest') = readHexFast $ trim  rest
				let (size,rest'') = readHexFast $   trim rest'
				let (ty,rest''') = readStringFast $! trim rest''
				let (tid, rest'''') = readHexFast $ trim rest'''
				
				allocate oid size ty tid m
	| firstChar == 'D' = do
				let (oid,rest') = readHexFast $ trim rest
				
				death oid m

	| firstChar == 'M' = do
				let (mid,rest') = readHexFast $ trim rest
				let (rid,rest'') = readHexFast $ trim rest'
				let (tid,rest''') = readHexFast $ trim rest''	

				methodEntry mid rid tid m
	| firstChar == 'E' = do
				let (mid, rest') = readHexFast $ trim rest
				let (rid, rest'') = readHexFast $ trim rest'
				let (tid, rest''') = readHexFast $ trim rest''
	
				methodExit mid rid tid m

	| firstChar == 'U' = do
				{--This is complicated because Updates can have am optional field id.
				   So they can look like:
					 U old origin new fieldID threadID 
				   or:   
					 U old origin new threadID
				   We handle both cases with a Maybe
                                --}
					
				let (old, rest') = readHexFast $ trim rest
				let (origin, rest'') = readHexFast $ trim rest'
				let (new, rest''') = readHexFast $ trim rest''
				let (fidOrTid,rest'''') = readHexFast $ trim rest'''
				--let (tid, rest''''') = readHexFast $ trim rest''''
				let maybeTid = readHex $ trim rest''''
				
				case maybeTid of 
					Just (tid,rest5) ->  update old origin new (Just fidOrTid) tid m
					Nothing          ->  update old origin new Nothing fidOrTid m


	| firstChar == 'R' = do
				let (rid,rest') = readHexFast $ trim rest
				let (tid, rest'') = readHexFast $ trim rest'

				root rid tid m
	| otherwise        = error $ "EtParser: Bad starting character" ++ [firstChar]
	where
	  firstChar = L.head s
          rest = L.tail s



