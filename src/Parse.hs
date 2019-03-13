{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.List.Split

import Types
import Config
import GHC.Prof

-- return a list of hot spot cost centers 
parseProfile :: String -> Double -> FilePath -> IO CCSrc 
parseProfile profMetric hotSpotThresh file = do
  text <- TLIO.readFile file
  case decode text of
    Left reason -> fail reason
    Right prof -> return $ foldr (eliminateCold profMetric hotSpotThresh) [] 
                         $ aggregatedCostCentres prof

-- filter out cold spots
eliminateCold :: String -> Double -> (AggregatedCostCentre -> CCSrc -> CCSrc) 
eliminateCold profMetric hotSpotThresh =
  case profMetric of 
    "MEM" -> (\cc ccSrc -> if (toRealFloat ((aggregatedCostCentreAlloc cc) / 100)) >= hotSpotThresh
                           then srcLine cc ccSrc
                           else ccSrc)
    "RT"  -> (\cc ccSrc -> if (toRealFloat ((aggregatedCostCentreTime cc) / 100)) >= hotSpotThresh
                           then srcLine cc ccSrc
                           else ccSrc)
    _     -> error $ "invalid profile metric type."    

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "no src locations generated from profile"

-- parse src location lines from cc and append to ccSrc vector
srcLine :: AggregatedCostCentre -> CCSrc -> CCSrc
srcLine cc ccSrc = do
    let src = T.unpack $ fromJust $ aggregatedCostCentreSrc cc
        split = splitOn ":" src
        count = length split 
    case count of 
        3 -> ((split !! 0), ((read (split !! 1) :: Int), (read (split !! 1) :: Int))) : ccSrc
	2 -> let ln1 = read ((splitOn "," ((splitOn "(" src) !! 1)) !! 0) :: Int
		 ln2 = read ((splitOn "," ((splitOn "(" src) !! 2)) !! 0) :: Int
             in ((split !! 0), (ln1, ln2)) : ccSrc
	_ -> ccSrc
