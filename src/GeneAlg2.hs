{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module GeneAlg2 where

import System.Random (mkStdGen, random, randoms, StdGen, randomR, getStdRandom)
import GA (Entity(..), GAConfig(..), evolve, evolveVerbose, randomSearch, Archive(..))
import Data.BitVector (BV, fromBits, toBits, size, ones)
import Data.Bits.Bitwise (fromListLE, fromListBE, toListBE)
import Data.Bits
import Data.List
import Control.DeepSeq
import Config
import Types
import Utils
import Rewrite (readBangs, editBangs)
--import qualified GeneAlg as Orig

import Data.Maybe (isJust, fromJust)
import System.Environment
import System.IO
import System.Process
import System.Directory hiding (executable)
import System.FilePath
import System.Random (StdGen)

import Result

--
-- GA TYPE CLASS IMPLEMENTATION
--

type Score = (Double, [Int])
type FitnessRun = [BV] -> IO Score

printBits :: [Bool] -> String
printBits = concatMap (\b -> if b then "1" else "0")

instance Entity [BV] [Int] (Double, FitnessRun) [[BV]] IO where

  -- Generate a random bang vector
  -- Invariant: pool is the vector with all bangs on
  --  genRandom pool seed = do {Just e <- mutation pool 0.4 seed pool; return $ e}
  genRandom pool seed = do
                           let g = mkStdGen seed
                               index = (fst $ randomR (0, (length pool) - 1) g) :: Int
                           print index
                           return $ pool !! index


  crossover pool p seed es1 es2 = do
                                    vecs <- sequence $ map (uncurry . uncurry $ crossoverSingle) $ (zip (zip es1 es2) $ head pool)
                                    return $! sequence vecs
                                  where
                                    crossoverSingle e1 e2 pool = do
                                      let len = size pool
                                          g = mkStdGen seed
                                          fs = take len $ randoms g :: [Float]
                                          bs = map (< 0.5) fs
                                          s = fromBits bs
                                          e1' = s .&. e1
                                          e2' = complement s .&. e2
                                          e' = e1' .|. e2'
                                      return $! Just e'

  -- Mutation operator
  mutation pool p seed es = do
                              vecs <- sequence $ map (mutateSingle) es
                              return $! sequence vecs
                          where
                            mutateSingle e = do
                              size e `seq` return $! Just e'
                                where
                                  len = size e
                                  g = mkStdGen seed
                                  fs = take len $ randoms g
                                  bs = map (< p) fs
                                  e' = e `xor` fromBits bs

  -- Improvement on base time
  -- NOTE: lower is better
  score (baseTime, fitRun) bangVecs = do
    (newTime, nBangs) <- fitRun bangVecs
    let score = if newTime >= 0 then (newTime / baseTime) else 2
        s' = (if (score >= 0 && score <= 1.05) then nBangs else (replicate (length nBangs) 1000))
    putStrLn $ "bits: " ++ (concat $ (map (printBits . toBits) bangVecs)) ++ ", " ++ (show nBangs) ++ ", " ++ (show score) ++ ", " ++ (show baseTime) ++ ". " ++ (show s')
    return $! Just s'

  showGeneration _ (_,archive) = "best: " ++ (show fit)
    where
      (Just fit, _) = head archive

--evolveProg :: StdGen -> GAConfig -> [BangVec] -> (Time, FitnessRun) -> IO (Archive [BangVec] Score)
evolveProg :: StdGen -> GAConfig -> [[BV]] -> (Double, FitnessRun) -> IO (Archive [BV] [Int])
evolveProg = GA.evolve

ev :: Cfg -> IO [([BangVec], Double)]
ev = undefined
{-
ev autobahnCfg = do
    let projDir = projectDir autobahnCfg
        cfg = createGAConfig autobahnCfg
        files = coverage autobahnCfg
        fitnessReps = fitnessRuns autobahnCfg
        baseTime  = getBaseTime autobahnCfg
        baseMetric  = getBaseMetric autobahnCfg

    checkBaseProgram baseTime baseMetric
    print baseTime
    print baseMetric

    let absPaths = map (\x -> projDir ++ "/" ++ x) files
        fitnessTimeLimit = deriveFitnessTimeLimit baseTime

  -- Pool: bit vector representing original progam
    progs <- sequence $ map readFile absPaths
    bs <- sequence $ map readBangs absPaths
    let !vecPool = rnf progs `seq` map fromBits bs

  -- Do the evolution!
    es <- evolve g cfg [vecPool] (baseMetric,
                                       fitness (autobahnCfg { getBaseTime = fitnessTimeLimit }) fitnessReps files)

    return $ map foo es
    where
       getScore s = case s of
                        Nothing -> error "filter should have removed all Nothing"
                        Just n -> n
       foo (Just (time), bv) = (bv, time)
       foo (Nothing, bv) = (bv, -1000)

-}
