{-# LANGUAGE BangPatterns #-}
import Simulate (simulate)
import qualified Data.ByteString.Lazy.Char8 as L
import ReachGC
import EtParser
import System (getArgs)
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Graph as G
import qualified Data.List as List
import Machine
import Data.Maybe (isJust, fromJust, fromMaybe)
import ObjectModel
import qualified Codec.Compression.GZip as GZip
import Data.Functor (fmap)
import Data.Foldable (toList)



gb = 1024*1024*1024
mb = 1024*1024

{--initMachine = Machine {
			       nursery = S.empty
                             , nursery_size = 0
                             , nursery_limit = 4*mb
                             , rem_set = S.empty
                             , heap_size = 0
                             , heap_limit = 4*gb --4 GB
                             , heap = G.empty
                             , roots = S.empty
                             , stacks = M.empty
                             , alloc = 0
                             , chunks = []
                             , marks = 0
            	           } --}




sum::S.IntSet->Int
sum = S.fold (+) 0 

mean::S.IntSet->Float
mean s = (fromIntegral $ Main.sum s) / (fromIntegral $ S.size s)


fMean::[Float]->Float
fMean fs = List.sum fs / (fromIntegral $ length fs)

stdDev::S.IntSet->Float
stdDev s = fMean $ map (\x -> (m-(fromIntegral x))^2) (S.toList s)
	  where
             m = mean s
{--
analyzeChunks m = fmap oneChunk $ chunks m
	where
	     cs = chunks m
	     oMap = objMap m
	     filterKnownDeath = S.filter (isJust.dTime) --Get those oids for which we know the death time
	     dTime oid = deathTime $ fromMaybe (error ("This failed some how" ++ show oid)) $  M.lookup oid oMap
	     oneChunk c = S.map (fromJust.dTime) $ filterKnownDeath  $  S.filter (\n-> n `M.member` oMap) c
--}
main = do
	 args <- getArgs
	 contents <- if(List.isSuffixOf ".gz" (args !! 0)) then
		       fmap GZip.decompress $ L.readFile (args !! 0)
		     else
			L.readFile (args !! 0)


	 --let !m = simulate initMachine $! map (f.readRecord) $! L.lines contents
	 let !m = List.foldl' readRecordMachine initMachine $! L.lines contents
	 let oMap = objMap m 
	 let !test = uniqueChunkMemb (toList $ chunks m)
	
	 --putStrLn ("Chunk uniqueness: " ++ (show test))
	 --putDChunks $ chunksToDeathTimeChunks (chunks m) (time m) oMap
	 putStrLn("oMap empty? " ++ show (M.null oMap) ++ "chunks empty?" ++ show (chunks m))
	 --putChunksCSV   (chunksToDeathTimeChunks (toList $ chunks m) (time m) oMap) 
	 putStrLn $ show m
	 where
	      f::Maybe (Record,L.ByteString) -> Record
	      f  (Just (r,_))   = r
	      f      _        = error "We are all going to die."


uniqueChunkMemb::[S.IntSet]->Bool
uniqueChunkMemb chunks = ucmSub S.empty chunks
	where
	  ucmSub s (c:cs) = ucmSub (S.fold check s c) cs
	     where
		    check obj s = if  obj `S.member` s then --Obj already seen in some other chunk
				  error ("Object " ++ (show obj) ++ "appears in multiple chunks!")
			        else
				  S.insert obj s


	  ucmSub s []  = True
	  	
{-chunksToDeathTimeChunks::[S.IntSet]-> Int->M.IntMap ObjectModel->[[Int]]
chunksToDeathTimeChunks cs endOfTime oMap = List.map sub cs
			     	  where
					sub::S.IntSet->[Int]
					sub ns = List.map dTime $ S.toList ns
					
					dTime::Int->Int
					dTime oid = fromMaybe endOfTime $ deathTime $ fromMaybe (error ("This failed some how" ++ show oid)) $  M.lookup oid oMap
-}

putChunk::[Int] -> IO ()
putChunk s = List.foldl' f  (return ()) s
	     where
		f io n = do
			   putStr $ show n
			   putStr "\n"
			   io


putDChunks::[[Int]]-> IO ()
putDChunks chunks = List.foldl' f (return ()) chunks
		    where
			f io c = do
				  putStr "===\n"
				  putChunk c 
				  io


--Output the chunks as comma seperated values, one chunk per column
putChunksCSV::[[Int]]->IO()
putChunksCSV cs = 
		if any (/= []) cs then
				do
 				  List.foldl' putD (return ()) $ map  showHead  cs
				  putStr "\n"
				  putChunksCSV  (map tailOrEmpty cs) 
			else
				  return ()
		where
		     showHead (i:is) = show i
		     showHead   []    = ""
		
		     tailOrEmpty (i:is) = is
		     tailOrEmpty   []   = []

		     putD io d = do
				  putStr d
				  putStr ", "
				  io
