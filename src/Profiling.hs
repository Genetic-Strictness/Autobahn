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

-- 
-- PROFILING EXTERNAL PROJECT
--

-- Build a cabal project. Project must be configured with cabal. `projDir` is in the current dir
buildProj :: FilePath -> IO ExitCode
buildProj projDir = system $ "cd " ++ projDir ++ "; cabal configure -v0; cabal build -v0"

statsFromMetric :: MetricType -> [(String, String)] -> Double
statsFromMetric RUNTIME stats = let Just muts = lookup "mutator_cpu_seconds" stats
                                    Just gcs = lookup "GC_cpu_seconds" stats
                                in read muts + read gcs
statsFromMetric ALLOC   stats = let Just bytes = lookup "bytes allocated" stats
                                in read bytes
statsFromMetric GC      stats = let Just gcs = lookup "GC_cpu_seconds" stats
                                in read gcs

benchmark :: Cfg -> Int64 -> IO (Double, Double)
benchmark (cfg @ Cfg
  { projectDir = projDir
  , timeBudget = timeLimit
  , fitnessMetric = metric
  }) runs = do
      putStrLn $ "Running: " ++ runProj
      buildExitC <- buildProj projDir
      case buildExitC of
        ExitSuccess -> executeProj
        _ -> return ((0 - 1), (0 - 1))
      where
        projDir = projectDir cfg
        mainFile = executable cfg -- TODO: more of coverage
        runProj = "timeout " ++ (show . round $ timeLimit) ++ "s " ++ projDir ++ "/dist/build/" 
                         ++ takeBaseName mainFile ++ "/" ++ takeBaseName mainFile
                         ++ " " ++ inputArgs cfg
                         ++ " -q +RTS -ttiming.temp --machine-readable"
                         ++ " > /dev/null"
        cleanProj = "rm -f timing.temp"
        executeProj = do 
          exitc <- system runProj 
          case exitc of
            ExitFailure _ -> return ((0 - 1), (0 - 1))
            ExitSuccess   -> do 
              !t <- readFile "timing.temp"
              system cleanProj
              let s = unlines . tail . lines $ t
                  stats = read s :: [(String, String)]
              runtime <- return $ statsFromMetric RUNTIME stats
              case metric of
                RUNTIME   -> return (runtime, runtime)
                otherwise -> return (runtime, statsFromMetric metric stats)

