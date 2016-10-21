module DeepSeqInstances where

import Control.DeepSeq
import Data.Foldable as F
import Data.IntMultiSet as MS
import Data.Sequence as Seq
import SmallIntSet as SmallS
import Graph
import qualified ObjectModel as OM



instance NFData Graph where
	rnf (Graph edgeMap) = edgeMap `deepseq` ()


instance NFData MS.IntMultiSet where
	rnf ms = let res = MS.fold s 0 ms
	         in
		     res `seq` ()
		 where
		   s i j = j `seq` i  


instance NFData IntList where
	rnf is = ilFoldl' f 0 is `seq` ()
		 where
		   f i j = i `seq` j `seq` j
		
instance NFData a => NFData (Seq.Seq a) where
	rnf s = (F.foldl' f  () s) `seq` ()
		where
		   f a b = a `seq` () 

instance NFData OM.ObjectModel where
        rnf om  =  (OM.size om) `deepseq` om `seq` ()
