{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Lazy.Char8 as L
import System (getArgs)
import qualified Data.List as List
import Data.Maybe (fromJust,isJust,fromMaybe)
import EtParser (trim, readHex)
import Data.Sequence as Seq
import Data.Foldable as F

--takes a two cloum list of numbers, returns % bigger than 1 MB, 4 MB, 8 MB, as well as mean, lower quartile, upper quartilep

sum::Seq.Seq Int->Int
sum = F.foldl' (+) 0 


mb::Int--number of bytes in a mega byte
mb = 1024*1024

--Note: Seq.length is O(1)
mean::Seq.Seq Int->Float
mean s = (fromIntegral $ Main.sum s) / (fromIntegral $ Seq.length s)

--For each time, how many things survived until that time?
--Assumes both times and lifeTimes are sorted
numSurvivedUntil::Int->[Int]->Seq.Seq Int->[Int]
numSurvivedUntil     _    []           _     = []
numSurvivedUntil surv_count (time:times) lifeTimes = if Seq.null lifeTimes || part_lt == -1 then
							 map (\x -> 0) (time:times) --There are no more objects to look through, so 0 objects survive until the remaining times
                	                              else
				           	         (surv_count'):(numSurvivedUntil surv_count' times survivors)
		   				      where
							 part_lt::Int
                     					 part_lt = fromMaybe (-1) $ Seq.findIndexL (\x -> x > time) lifeTimes
							 survivors::Seq.Seq Int --The ones who surived this around
							 survivors = Seq.drop (part_lt) lifeTimes
							 --Total number of survivors in all rounds for far
						         surv_count' = surv_count - (part_lt) 


main = do
         args <- getArgs
	 contents <- L.readFile (args !! 0)

	 --let sizes = F.foldl' (+) 0 $ Seq.fromList  $ map (fromJust.readSize) $ L.lines contents
	 --let (lower, mean, lquart,uquart) = quantify sizes
	 let !lifeTimes = if List.isSuffixOf ".sorted" (args !! 0) then --If it is already sorted, we can save time
			      Seq.fromList $! map (fromJust.readSize) $ L.lines contents
			  else
			     Seq.unstableSort $ Seq.fromList $! map (fromJust.readSize) $ L.lines contents

	 let !res = map (\x -> (fromIntegral x)/(fromIntegral $ Seq.length lifeTimes)) $! numSurvivedUntil (Seq.length lifeTimes) [1*mb,2*mb,4*mb,100*mb] lifeTimes
	 putStrLn ("---survival results for" ++ (args !! 0) ++ "---")
	 putStrLn ("1 mb survival fraction: " ++ (show (res !! 0)))
	 putStrLn ("2 mb survival fraction: " ++ show (res !! 1))
	 putStrLn ("4 mb survival fraction: " ++ show (res !! 2))
	 putStrLn ("100 mb survival fraction: " ++ show (res !! 3))

	 --putStrLn $ "mean liftime: " + show mean
	
readSize::L.ByteString -> Maybe Int
readSize s = do
	       (oid,rest) <- readHex s
               (lifetime,_) <- L.readInt $ trim rest

	       Just lifetime
