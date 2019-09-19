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
pathToNoFib = "/data/marilyn"

-- Build a cabal project. Project must be configured with cabal. `projDir` is in the current dir
buildProj :: FilePath -> IO ExitCode

-- For cabal files
buildProj projDir = system $ "cd " ++ projDir ++ "; cabal configure -v0 --enable-profiling; cabal build -v0"

{-
-- For nofib makefile specifically
buildProj projDir = do
  setCurrentDirectory projDir
  putStrLn projDir
  system "make clean; make boot" -- &> /dev/null"
-}

statsFromMetric :: MetricType -> [(String, String)] -> Double
statsFromMetric RUNTIME stats = let Just muts = lookup "mut_cpu_seconds" stats
                                    Just gcs = lookup "GC_cpu_seconds" stats
                                in read muts + read gcs
statsFromMetric ALLOC   stats = let Just bytes = lookup "allocated_bytes" stats
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
        mainFile = executable cfg
        runProj = "cd " ++ projDir ++ "; cabal build -v0; timeout " ++ (show . round $ timeLimit)
            ++ "s cabal run " ++ takeBaseName mainFile
                        ++ " -- " ++ inputArgs cfg
                        ++ " +RTS -pa -ttiming.temp --machine-readable"
                        ++ " > /dev/null && sleep 1"

{-
-- For nofib makefile specifically
        runProj = "make -k mode=norm > nofib-gen 2>&1"
-}
        cleanProj = "cd " ++ projDir ++ "; cabal clean; rm -f timing.temp"
        executeProj = do
          d <- getCurrentDirectory
          putStrLn d
          putStrLn $ "Running: " ++ runProj
          exitc <- system $ runProj
          case exitc of
            ExitFailure _ -> return ((0 - 1), (0 - 1))

{-
-- For nofib makefile specifically
            ExitSuccess   -> do
                                          system $ pathToNoFib ++ "/nofib/nofib-analyse/nofib-analyse --csv=Runtime nofib-gen nofib-gen > temp.prof"

                               fc <- readFile "temp.prof"
                               let wcs = words $ map (\c -> if c == ',' then ' ' else c) fc
                               return ((read $ wcs !! 1), (read $ wcs !! 1))
-}

-- For cabal files
            ExitSuccess   -> do
              !t <- readFile $ projDir ++ "/" ++ "timing.temp"
              system cleanProj
              system $ "cd " ++ d
              let s = unlines . tail . lines $ t
                  stats = read s :: [(String, String)]
              runtime <- return $ statsFromMetric RUNTIME stats
              print runtime
              case metric of
                RUNTIME   -> return (runtime, runtime)
                otherwise -> return (runtime, statsFromMetric metric stats)
