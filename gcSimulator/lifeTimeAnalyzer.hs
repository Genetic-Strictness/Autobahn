{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString.Lazy.Char8 as L
import EtParser
import Machine
import ObjectModel
import qualified Data.IntMap as M
import Data.IntMap ((!))
import System (getArgs)
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import qualified  Data.Map as Map
import qualified Data.Foldable as F
import qualified Codec.Compression.GZip as GZip
import Data.Maybe (fromJust, fromMaybe)
import qualified ObjectToTypeMap as OTT



simStep::(Machine, OTT.ObjectToTypeMap)->Record->(Machine,OTT.ObjectToTypeMap)
simStep (!m,!oid2type) (Alloc oid size ty _ ) = (m{objMap=objMap'},oid2type')
				 where
				  objMap' = M.insert  oid (ObjectModel (time m) Nothing size) (objMap m)
				  oid2type' = OTT.insert oid ty oid2type
				
simStep (!m,!oid2type) (Entry mid _ tid) = (m{time = (time m) + 1},oid2type)
simStep (!m,!oid2type) (Exit mid _ tid)  = (m{time= (time m) +1},  oid2type)
simStep (!m,!oid2type) (Death oid)       = (m{objMap=objMap'},     oid2type)
			where
     			  objMap' =M.adjust (\(ObjectModel aTime _ size)->ObjectModel aTime (Just $ time m) size  ) oid $ objMap m
simStep moid2type _ = moid2type


simulate rs = List.foldl' simStep (initMachine{heap_limit=100*Machine.mb},OTT.empty) rs

main = do
	 args <- getArgs

         contents <- if(List.isSuffixOf ".gz" (args !! 0)) then
                       fmap GZip.decompress $ L.readFile (args !! 0)
                     else
                        L.readFile (args !! 0)

	 let (m,!oid2type) = simulate $! List.map (f.readRecord) $! L.lines contents
	 let objMap' = M.map (nothingToEndOfTime (time m)) (objMap m)
	 let tlist = groupByType oid2type

	 --putStrLn $ showMap $  meanLifeTimeByType objMap'  tlist
	 putStrLn $ show tlist	


	 where
	      f::Maybe (Record,L.ByteString) -> Record
	      f  (Just (r,_))   = r
	      f      _        = error "We are all going to die."

	      nothingToEndOfTime endOfTime objModel  = if deathTime objModel == Nothing then
						        objModel{deathTime = Just $ endOfTime}
					              else
						        objModel




putMap:: Map.Map L.ByteString Float -> IO ()
putMap m = Map.foldWithKey putKV (return ()) m
	   where
	     putKV k v io = do
			     putStr $ L.unpack k
			     putStr " "
			     putStr $ show v
			     putStr "\n"
			     io	

showMap m= Map.foldWithKey  f "" m
	   where
		f k v s= s ++ L.unpack k ++ " " ++  show v ++ "\n"


meanLifeTimeByType::M.IntMap ObjectModel->Map.Map L.ByteString (Seq.Seq Int)->Map.Map L.ByteString Float
meanLifeTimeByType omap  tmap =  Map.map meanLife tmap
				 where
				   meanLife:: (Seq.Seq Int)->Float
				   meanLife oids = mean $ fmap  lifeTime oids

				   lifeTime::Int->Int
				   lifeTime oid = (fromJust $ (deathTime (omap ! oid))) - ( allocTime  (omap ! oid))



sum::Seq.Seq Int->Float
sum s = F.foldl' (\x y-> x +(fromIntegral y)) 0.0 s

mean s = Main.sum s / (fromIntegral $ Seq.length s)

groupByType::OTT.ObjectToTypeMap->Map.Map L.ByteString (Seq.Seq Int)
{--groupByType oid2type = M.foldWithKey f  Map.empty  oid2type
		       where
			 f::Int->L.ByteString->Map L.ByteString (Seq.Seq Int)->Map L.ByteString (Seq.Seq Int)
			 f !oid !ty !m = Map.alter (g oid) ty m 

			 g::Int->Maybe (Seq.Seq Int)->Maybe (Seq.Seq Int)
			 g  !oid Nothing =  Just $ Seq.singleton oid
			 g  !oid (Just seq) = Just (oid <| seq)--}

groupByType oid2type = List.foldl' f Map.empty $ OTT.toList oid2type
		       where
			 f !m (oid,ty) = Map.alter g ty m
				where 
			 	  g Nothing =  Just $ Seq.singleton oid
				  g (Just seq) = Just (oid <| seq)
			


 

