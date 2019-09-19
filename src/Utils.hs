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
import Data.List
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

numBangs :: [(Bool, Int)] -> Int
numBangs bs = foldr (\n count -> if fst n then count + 1 else count) 0 bs

numPats :: [[Bool]] -> Int
numPats bs = foldl (\acc x -> acc + (length x)) 0 bs

countBangs :: [[Bool]] -> Int
countBangs bv = foldl (\acc f -> acc + count f) 0 bv
                    where count f = foldr (\n acc -> if n then acc + 1 else acc) 0 f

checkBaseProgram :: Double -> Double -> IO ()
checkBaseProgram baseTime baseMetric =
    if baseTime < 0
    then error $ "Base program ran longer than expected. " ++
                 "We suggest a larger time budget."
    else if baseMetric <= 0
         then error $ "Base measurement is negligible (0). " ++
                      "Autobahn cannot optimize."
         else putStrLn $ "Base measurement is: " ++ (show baseTime)

-- given list of files to optimize and list of files in directory,
-- returns list of files to optimize if there are any
checkFiles :: [[Char]] -> [[Char]] -> IO [[Char]]
checkFiles files fsInDir = do
    if length files == 0
    then error $ "There are no files to be optimized."
    else do
         let extraFiles = files \\ fsInDir
         if (length extraFiles) > 0
         then putStrLn $ "To improve Autobahn performance,"
                          ++ "add the following files to your directory: "
                          ++ (show extraFiles)
         else print ""
         return extraFiles

checkRunTimeError :: Double -> IO()
checkRunTimeError time =
    if time < 0
    then error $ "Invalid negative runtime - ERROR"
    else putStr $ "Valid runtime."

checkImprovement :: Double -> Double -> IO()
checkImprovement surBaseTime baseTime =
    if (((baseTime - surBaseTime)/baseTime) <= 0.06)
       || surBaseTime < 0
    then error $ "Phase 1 improvement is negligible."
    else putStr $ "improvement is noticeable."

countSetBits :: [BangVec] -> [Int]
countSetBits bangs =
    map countSetBitsSingle bangs
    where
        countSetBitsSingle = length . (filter id) . B.toBits

fitness :: Cfg -> Int64 -> [FilePath] -> [BangVec] -> IO (Time, [Int])
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
    let n_ones = countSetBits bangVecs
    return (newMetricStat, n_ones)

fitnessNBangs :: Cfg -> Int64 -> [FilePath] -> [BangVec] -> IO [Int]
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
    let n_ones = countSetBits bangVecs
    return (if newMetricStat < 0 then (replicate 1000 $ length bangVecs) else n_ones)
