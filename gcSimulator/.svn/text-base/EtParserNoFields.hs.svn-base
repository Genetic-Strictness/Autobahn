module EtParserNoFields where


import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as S
import qualified Data.Char as C
import qualified Graph as G
import qualified Data.Bits as Bits
import Data.List as List



data Record =  Alloc !Int !Int L.ByteString !Int
	     | Update !Int !Int !Int !Int 
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


--trim leading white space 
trim::L.ByteString -> L.ByteString
trim  = L.dropWhile C.isSpace 


readString::L.ByteString -> Maybe (L.ByteString,L.ByteString)
readString s = Just ( L.copy $ L.takeWhile (notSpace) s , L.dropWhile (notSpace) s)
		where
			notSpace = not.C.isSpace




readRecord::L.ByteString -> Maybe (Record, L.ByteString)
readRecord s
	| firstChar == 'A' =  do
				(oid,rest') <- readHex $ trim  rest
				(size,rest'') <- readHex $   trim rest'
				(ty,rest''') <- readString $! trim rest''
				(tid, rest'''') <- readHex $ trim rest'''
				
				return (Alloc oid size ty tid, rest'''')
	| firstChar == 'D' = do
				(oid,rest') <- readHex $ trim rest
				
				return (Death oid, rest')

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
				(old, rest') <- readHex $ trim rest
				(origin, rest'') <- readHex $ trim rest'
				(new, rest''') <- readHex $ trim rest''
				(tid, rest'''') <- readHex $ trim rest'''
				

				return (Update old origin new tid, rest'''')

	| firstChar == 'R' = do
				(rid,rest') <- readHex $ trim rest
				(tid,rest'') <- readHex $ trim rest'

				return (Root rid tid, rest'')
				
	where
		firstChar = L.head s
		rest =	L.tail s



