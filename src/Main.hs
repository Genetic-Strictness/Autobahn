{-# LANGUAGE BangPatterns #-}

import Types
import Rewrite
import Profiling
import qualified GeneAlg as G1
import qualified GeneAlg2 as G2
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
import System.Directory hiding (executable)
import Text.Read
import Control.DeepSeq
import System.FilePath

import Debug.Trace
import System.Random (StdGen)
import Data.Maybe (isJust, fromJust)

import Data.List

reps :: Int64
reps = runs

cfgFile :: FilePath
cfgFile = "config.atb"

performEvolve :: Cfg -> IO [([BangVec], Double)]
performEvolve cfg = let alg = algorithm cfg in
                    case alg of
                      ORIGINAL -> G1.ev cfg
--                      MINIMIZE -> G2.ev cfg
                      otherwise -> error $ "Unkown algorithm type: " ++ (show alg)

main :: IO () 
main = do 
  hSetBuffering stdout LineBuffering
  args <- getArgs
  cfg <- if null args then gatherCfg else manufactureCfg args
  putStrLn "Setting up optimization process..."
  putStrLn "Starting optimization process..."
  gmain cfg
  putStrLn $ "Optimization finished, please inspect and select candidate changes "
   ++ "(found in AutobahnResults under project root)"
      where
          
          gatherCfg = do 
            putStrLn "Configure optimization..."
            cfgExist <- doesFileExist cfgFile
            cfg <- if cfgExist then readCfg cfgFile else cliCfg
            return cfg
          manufactureCfg args = do 
                                     [a, b, c, d] <- getArgs
                                     (baseTime, baseMetric) <- benchmark (defaultCfgWithProjectDir a) 1
                                     allFiles <- getDirectoryContents a
                                     print (baseTime, baseMetric)
                                     return Cfg { projectDir = a
                                                , timeBudget = deriveFitnessTimeLimit baseTime
                                                , getBaseTime = baseTime
                                                , coverage = filter (isSuffixOf ".hs") allFiles
                                                , executable = ""
                                                , fitnessMetric = RUNTIME
                                                , getBaseMetric = baseMetric
                                                , pop = (read b)
                                                , gen = (read c)
                                                , arch = (read d)
                                                , fitnessRuns = 1
                                                , inputArgs = ""
                                                , algorithm = ORIGINAL
                                                }
                                      

gmain :: Cfg -> IO ()
gmain autobahnCfg = do
    let projDir = projectDir autobahnCfg
        cfg = createGAConfig autobahnCfg
        files = coverage autobahnCfg
        fitnessReps = fitnessRuns autobahnCfg
        baseTime  = getBaseTime autobahnCfg
        baseMetric  = getBaseMetric autobahnCfg
    putStrLn $ "Optimizing " ++ projDir
    putStrLn $ ">>>>>>>>>>>>>>>START OPTIMIZATION>>>>>>>>>>>>>>>"
    putStrLn $ "pop: " ++ (show $ pop autobahnCfg)
    putStrLn $ "gens: " ++ (show $ gen autobahnCfg)
    putStrLn $ "arch: " ++ (show $ arch autobahnCfg)


    results <- performEvolve autobahnCfg
    return ()

-- DIO: Do not delete anything below here. I need to figure out how to bring the Autobahn + minimizer
--      setup into this API. Currently, I need two different instances of the same Entity typeclass to
--      do this. Will need to figure out a good fix for this.

{-    
    checkBaseProgram baseTime baseMetric
    print baseTime
    print baseMetric
    
    let absPaths = map (\x -> projDir ++ "/" ++ x) files
        fitnessTimeLimit = deriveFitnessTimeLimit baseTime

  -- Pool: bit vector representing original progam
    progs <- sequence $ map readFile absPaths
    bs <- sequence $ map readBangs absPaths
    let !vecPool = rnf progs `seq` map B.fromBits bs

  -- Do the evolution!
    let ev = evolve :: StdGen -> GAConfig -> [BangVec] -> (Time, FitnessRun) -> IO (Archive [BangVec] Score)
    es <- ev g cfg vecPool (baseMetric,
                                       fitness (autobahnCfg { getBaseTime = fitnessTimeLimit }) fitnessReps files)
    let e = snd $ head es :: [BangVec]
        score = case (fst $ head es) of
                   Just s -> s
                   Nothing -> (2.0, []) :: Score
        atbResult = if (fst score) > 1 then vecPool else e

    progs' <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits e)

    let bestMetricPerc = fst $ fromJust $ fst $ head $ filter (\x -> isJust $ fst x) es

    print bestMetricPerc

    newEs <- G2.ev g cfg [atbResult] (baseMetric,
                                   fitness (autobahnCfg { getBaseTime = fitnessTimeLimit }) fitnessReps files)
    let e2 = snd $ head newEs :: [BangVec]
        num_orig_bangs = countSetBits atbResult
        minResult = case (fst $ head newEs) of
                      Just s -> if s < num_orig_bangs then e2 else atbResult
                      Nothing -> atbResult

  -- Write the original files back to disk
    sequence $ map (uncurry writeFile) $ zip absPaths progs

  -- Debug: Print out actual ATB and Minimizer results
    putStrLn $ "ATB Result: " ++ (unlines $ (map (printBits . B.toBits) e))
    putStrLn $ "Min Result: " ++ (unlines $ (map (printBits . B.toBits) e2))

  -- Write result
    putStrLn $ "best entity (GA): " ++ (unlines $ (map (printBits . B.toBits) atbResult))

    putStrLn $ "best entity (GA) - r2: " ++ (unlines $ (map (printBits . B.toBits) minResult))
    
    let newPath = projDir ++ "/" ++ "autobahn-survivor"
    code <- system $ "mkdir -p " ++ newPath
    
    let survivorPaths = map (\x -> projDir ++ "/" ++ "autobahn-survivor/" ++ x) files

    let survivorDirs = map (takeDirectory) survivorPaths
    sequence $ map (\x -> system $ "mkdir -p " ++ x) survivorDirs
    sequence $ map (uncurry writeFile) $ zip survivorPaths progs'
    putStrLn ">>>>>>>>>>>>>>FINISH OPTIMIZATION>>>>>>>>>>>>>>>"

      -- Write result page
    let es' = filter (\x -> fst x /= Nothing) es
        bangs = (map snd es') :: [[BangVec]]
    newFps <- createResultDirForAll projDir absPaths bangs
    let f = map fst es'
        scores = map getScore f
    genResultPage projDir (map fst scores) newFps projDir Nothing cfg 0.0 1

    -- Write the minimizer results to a ".min" file
    let minAbsPaths = map (\x -> projDir ++ "/" ++ x) (map (\x -> x ++ ".opt") files)
    !atb_progs <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits atbResult) 
    rnf atb_progs `seq` sequence_ $ map (uncurry writeFile) $ zip minAbsPaths atb_progs

    -- Write the minimizer results to a ".min" file
    let minAbsPaths = map (\x -> projDir ++ "/" ++ x) (map (\x -> x ++ ".min") files)
    !min_progs <- sequence $ map (uncurry editBangs) $ zip absPaths (map B.toBits minResult) 
    rnf min_progs `seq` sequence_ $ map (uncurry writeFile) $ zip minAbsPaths min_progs

    -}

    where
       getScore s = case s of
                        Nothing -> error "filter should have removed all Nothing"
                        Just n -> n

