{-# LANGUAGE BangPatterns #-}

module Utils where

import Types
import Rewrite
import Profiling
import Config
import Result
------
import GA
import qualified Data.BitVector as B
import Data.Bits
import Data.Int
import System.Environment
import System.IO
import System.Process
import System.Directory
import Text.Read
import Control.DeepSeq
import System.FilePath

import Debug.Trace
import System.Random (StdGen)
import Data.Maybe (isJust, fromJust)

reps :: Int64
reps = runs

checkBaseProgram :: Double -> Double -> IO ()
checkBaseProgram baseTime baseMetric = if baseTime < 0
                                       then error $ "Base program ran longer than expected. " ++
                                                  "We suggest a larger time budget."
                                       else if baseMetric <= 0
                                            then error $ "Base measurement is negligible (0). " ++
                                                 "Autobahn cannot optimize."
                                            else putStr $ "Base measurement is: " ++ (show baseTime)

fitness :: Cfg -> Int64 -> [FilePath] -> [BangVec] -> IO (Time, Int)
fitness cfg reps files bangVecs = do
  -- Read original
    let absPaths = map (\x -> projectDir cfg ++ "/" ++ x) files
    !progs  <- sequence $ map readFile absPaths
  -- Rewrite from gene
    !progs' <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits bangVecs) 
    rnf progs `seq` sequence $ map (uncurry writeFile) $ zip absPaths progs'
  -- Benchmark new
    -- buildProj projDir
    !(_, newMetricStat) <- benchmark cfg reps
  -- Recover original
    !_ <- sequence $ map (uncurry writeFile) $ zip absPaths progs
    let n_ones = length . (filter (\x -> x)) . B.toBits $ head bangVecs
    return (newMetricStat, n_ones)

fitnessNBangs :: Cfg -> Int64 -> [FilePath] -> [BangVec] -> IO Int
fitnessNBangs cfg reps files bangVecs = do
  -- Read original
    let absPaths = map (\x -> projectDir cfg ++ "/" ++ x) files
    !progs  <- sequence $ map readFile absPaths
  -- Rewrite from gene
    !progs' <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits bangVecs) 
    rnf progs `seq` sequence $ map (uncurry writeFile) $ zip absPaths progs'
  -- Benchmark new
    -- buildProj projDir
    !(_, newMetricStat) <- benchmark cfg reps
  -- Recover original
    !_ <- sequence $ map (uncurry writeFile) $ zip absPaths progs
    let n_ones = length . (filter (\x -> x)) . B.toBits $ head bangVecs
    return (if newMetricStat < 0 then 1000 else n_ones)
