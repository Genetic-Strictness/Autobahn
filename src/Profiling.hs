{-# LANGUAGE BangPatterns #-}

module Profiling where

import Types
------
import Control.DeepSeq
import Data.Int
import System.Process
import System.Exit
import System.Timeout
import GHC.Stats
import System.FilePath.Posix (takeFileName, takeBaseName)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
-- 
-- PROFILING EXTERNAL PROJECT
--

pathToNoFib :: String
pathToNoFib = "/data/dan"

-- Build a cabal project. Project must be configured with cabal. `projDir` is in the current dir
buildProj :: FilePath -> IO ExitCode
buildProj projDir = system $ "cd " ++ projDir ++ "; cabal configure -v0; cabal build -v0"

{-
-- For nofib makefile specificallybuildProj projDir = do 
  setCurrentDirectory projDir
  system "make clean -s; make boot -s &> /dev/null"
-}

statsFromMetric :: MetricType -> [(String, String)] -> Double
statsFromMetric RUNTIME stats = let Just muts = lookup "mutator_cpu_seconds" stats
                                    Just gcs = lookup "GC_cpu_seconds" stats
                                in read muts + read gcs
statsFromMetric ALLOC   stats = let Just bytes = lookup "bytes allocated" stats
                                in read bytes
statsFromMetric GC      stats = let Just gcs = lookup "GC_cpu_seconds" stats
                                in read gcs

benchmark :: Cfg -> Int64 -> IO (Double, Double)
benchmark !(cfg @ Cfg
  { projectDir = projDir
  , timeBudget = timeLimit
  , fitnessMetric = metric
  }) runs = do
      buildExitC <- buildProj projDir
      case buildExitC of
        ExitSuccess -> executeProj
        _ -> return ((0 - 1), (0 - 1))
      where
        projDir = projectDir cfg
        mainFile = executable cfg -- TODO: more of coverage
{-        runProj = "timeout " ++ (show . round $ timeLimit) ++ "s " ++ projDir ++ "/dist/build/" 
                         ++ takeBaseName mainFile ++ "/" ++ takeBaseName mainFile
                         ++ "bintree" ++ "/" ++ "bintree"
                         ++ " " ++ inputArgs cfg
                         ++ " -q +RTS -ttiming.temp --machine-readable"
                         ++ " > /dev/null"
-}
        runProj = "cd " ++ projDir ++ " && bash run.sh Main"
{-
-- For nofib makefile specifically
        runProj = "make -k mode=norm > nofib-gen 2>&1"
-}
        cleanProj = "rm -f timing.temp"
        executeProj = do 
          putStrLn $ "Running: " ++ runProj
          d <- getCurrentDirectory
          putStrLn d
          exitc <- system $ runProj 
          case exitc of
            ExitFailure _ -> return ((0 - 1), (0 - 1))
{-
-- Getting timing for nofib
            ExitSuccess   -> do
       	    		       system $ pathToNoFib ++ "/nofib/nofib-analyse/nofib-analyse --csv=Runtime nofib-gen nofib-gen > temp.prof" -- TODO heuristcs hardcoded
			       
       	    		       -- TODO dirty hack here! abusing nofib-analyse
			       fc <- readFile "temp.prof"
			       let wcs = words $ map (\c -> if c == ',' then ' ' else c) fc
			       -- system "rm nofib-gen; rm temp.prof"
			       return ((read $ wcs !! 1), (read $ wcs !! 1))
-}
            ExitSuccess   -> do 

              !t <- readFile $ projDir ++ "/" ++ "timing.temp"
              system cleanProj
              let s = unlines . tail . lines $ t
                  stats = read s :: [(String, String)]
              runtime <- return $ statsFromMetric RUNTIME stats
              case metric of
                RUNTIME   -> return (runtime, runtime)
                otherwise -> return (runtime, statsFromMetric metric stats)

