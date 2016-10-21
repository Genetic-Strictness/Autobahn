module SmallIntSet (SmallIntSet, union, insert, toIntSet, empty, rnf, member) where
import qualified Data.IntSet as S
import qualified Control.DeepSeq
import Control.DeepSeq

data SmallIntSet =   Empty 
		   | SingSet {-# UNPACK #-} !Int
		   | DoubSet {-# UNPACK #-} !Int {-# UNPACK #-} !Int
		   | TripSet {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
		   | Full !S.IntSet
		   deriving (Show,Eq)

empty::SmallIntSet
empty = Empty


member::Int->SmallIntSet->Bool
member i  Empty		 = False
member i (SingSet j)     = i == j
member i (DoubSet j k)   = i == j || i == k
member i (TripSet j k l) = i == j || i == k || i == l
member i (Full   s)      = i `S.member` s


toIntSet::SmallIntSet->S.IntSet
toIntSet Empty           = S.empty
toIntSet (SingSet i)     = S.singleton i
toIntSet (DoubSet j k)   = S.fromList [j,k]
toIntSet (TripSet j k l) = S.fromList [j,k,l]
toIntSet (Full s)        = s

union::SmallIntSet->SmallIntSet->SmallIntSet
union    s         Empty         =   s
union  Empty         s           =   s
union (SingSet i) (SingSet j)
		   |i == j       = (SingSet i)
		   |otherwise    = (DoubSet i j)
union (SingSet i)  d@(DoubSet j k) --By construction, j /=k
		   | i == j       = d
		   | i == k       = d
		   | otherwise   = TripSet i j k
union (SingSet i) t@(TripSet j k l) -- j /= k /= l
		  | i == j       = t
		  | i == k       = t
		  | i == l       = t
union (SingSet i) (Full s)       = Full (S.insert i s)


union d@(DoubSet i j) (SingSet k)  
		  | i  == k      = d
		  | j  == k      = d
		  | otherwise    = (TripSet i j k)

union d@(DoubSet i j) (DoubSet k l)
		| (i == k && j == l) || (i == l && j ==k)  = d --Equivalent sets
		| (i == k)                                 = TripSet i j l
		| (i == l)                                 = TripSet i j k
		| otherwise                                = Full $ S.fromList [i,j,k,l]

union       s1        s2        =  Full $ (toIntSet s1) `S.union` (toIntSet s2)

		 
insert::Int->SmallIntSet->SmallIntSet
insert i Empty      = SingSet i
insert i s@(SingSet j) 
	  |  i == j = s
	  |  otherwise = DoubSet i j
insert i s@(DoubSet j k) 
	  |  i == j || i == k = s
	  | otherwise   = TripSet i j k
insert i s@(TripSet j k l)
	  | i == j || i == k || i == l = s
	  | otherwise = Full $ S.insert i (toIntSet s)
insert i s@(Full is) = Full $ S.insert i is


instance NFData SmallIntSet where
	rnf (SingSet i)     = seq i ()
	rnf (DoubSet i j)   = i `seq` j `seq` ()
	rnf (TripSet i j k) = i `seq` j `seq` k `seq` ()
	rnf (Full s)        = s `deepseq` ()
