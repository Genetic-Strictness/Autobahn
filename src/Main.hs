{-# LANGUAGE BangPatterns #-}

import Rewrite
import Profiling
import GeneAlg
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

reps :: Int64
reps = runs

cfgFile :: FilePath
cfgFile = "config.atb"

checkBaseProgram :: Double -> Double -> IO ()
checkBaseProgram baseTime baseMetric = if baseTime == -1
                                       then error $ "Base program ran longer than expected. " ++
                                                  "We suggest a larger time budget."
                                       else if baseMetric <= 0
                                            then error $ "Base measurement is negligible (0). " ++
                                                 "Autobahn cannot optimize."
                                            else putStr $ "Base measurement is: " ++ (show baseTime)

fitness :: FilePath -> String -> Double -> Int64 -> MetricType -> [FilePath] -> [BangVec] -> IO Time
fitness projDir args timeLimit reps metric files bangVecs = do
  -- Read original
    let absPaths = map (\x -> projDir ++ "/" ++ x) files
    !progs  <- sequence $ map readFile absPaths
  -- Rewrite from gene
    !progs' <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits bangVecs) 
    rnf progs `seq` sequence $ map (uncurry writeFile) $ zip absPaths progs'
  -- Benchmark new
    -- buildProj projDir
    !(_, newMetricStat) <- benchmark projDir args timeLimit metric reps
  -- Recover original
    !_ <- sequence $ map (uncurry writeFile) $ zip absPaths progs
    return newMetricStat
    
main :: IO () 
main = do 
  hSetBuffering stdout LineBuffering
  putStrLn "Configure optimization..."
  cfgExist <- doesFileExist cfgFile
  cfg <- if cfgExist then readCfg cfgFile else cliCfg
  putStrLn "Setting up optimization process..."
  putStrLn "Starting optimization process..."
  gmain cfg
  putStrLn $ "Optimization finished, please inspect and select candidate changes "
        ++ "(found in AutobahnResults under project root)"

gmain :: Cfg -> IO ()
gmain autobahnCfg = do
    let projDir = projectDir autobahnCfg
        cfg = createGAConfig autobahnCfg
        metric = fitnessMetric autobahnCfg
        files = coverage autobahnCfg
        fitnessReps = fitnessRuns autobahnCfg
        args = inputArgs autobahnCfg
        baseTime  = getBaseTime autobahnCfg
        baseMetric  = getBaseMetric autobahnCfg
    putStrLn $ "Optimizing " ++ projDir
    putStrLn $ ">>>>>>>>>>>>>>>START OPTIMIZATION>>>>>>>>>>>>>>>"
    putStrLn $ "pop: " ++ (show $ pop autobahnCfg)
    putStrLn $ "gens: " ++ (show $ gen autobahnCfg)
    putStrLn $ "arch: " ++ (show $ arch autobahnCfg)
    
    checkBaseProgram baseTime baseMetric
    
    let absPaths = map (\x -> projDir ++ "/" ++ x) files
        fitnessTimeLimit = deriveFitnessTimeLimit baseTime

  -- Pool: bit vector representing original progam
    progs <- sequence $ map readFile absPaths
    bs <- sequence $ map readBangs absPaths
    let !vecPool = rnf progs `seq` map B.fromBits bs

  -- Do the evolution!
    es <- evolveVerbose g cfg vecPool (baseMetric,
                                       fitness projDir args fitnessTimeLimit fitnessReps metric files)
    let e = snd $ head es :: [BangVec]
    progs' <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits e)

  -- Write the original files back to disk
    sequence $ map (uncurry writeFile) $ zip absPaths progs

  -- Write result
    putStrLn $ "best entity (GA): " ++ (unlines $ (map (printBits . B.toBits) e))
    newPath <- return $ projDir ++ "/" ++ "autobahn-survivor"
    code <- system $ "mkdir -p " ++ newPath
    
    let survivorPaths = map (\x -> projDir ++ "/" ++ "autobahn-survivor/" ++ x) files

    let survivorDirs = map (takeDirectory) survivorPaths
    sequence $ map (\x -> system $ "mkdir -p " ++ x) survivorDirs
    sequence $ map (uncurry writeFile) $ zip survivorPaths progs'
    putStrLn ">>>>>>>>>>>>>>FINISH OPTIMIZATION>>>>>>>>>>>>>>>"

      -- Write result page
    es' <- return $ filter (\x -> fst x /= Nothing) es
    bangs <- return $ (map snd es') :: IO [[BangVec]]
    newFps <- createResultDirForAll projDir absPaths bangs
    f <- return $ map fst es'
    scores <- return $ map getScore f
    genResultPage projDir scores newFps projDir Nothing cfg 0.0 1

    where
       getScore s = case s of
                        Nothing -> error "filter should have removed all Nothing"
                        Just n -> n
