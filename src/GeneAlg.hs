{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GeneAlg where

import System.Random (mkStdGen, random, randoms)
import GA (Entity(..), GAConfig(..), evolveVerbose, randomSearch)
import Data.BitVector (BV, fromBits, toBits, size, ones)
import Data.Bits.Bitwise (fromListLE, fromListBE, toListBE)
import Data.Bits
import Data.List
import Control.DeepSeq
import Config

--
-- GA TYPE CLASS IMPLEMENTATION
--

type BangVec = BV 
type Time = Double
type Score = Double
type FitnessRun = [BangVec] -> IO Score

-- TODO: implement in case we ever need to parse Bit (Bang) Vectors
readsBV = undefined

instance Read BangVec where -- TODO is this ok?
  readsPrec _ s = readsBV

printBits :: [Bool] -> String
printBits = concatMap (\b -> if b then "1" else "0")

instance Entity [BangVec] Score (Time, FitnessRun) [BangVec] IO where
 
  -- Generate a random bang vector
  -- Invariant: pool is the vector with all bangs on
  genRandom pool seed = do {Just e <- mutation pool 0.4 seed pool; return $ e}
  
  crossover pool p seed es1 es2 = do
                                    vecs <- sequence $ map (uncurry . uncurry $ crossoverSingle) $ (zip (zip es1 es2) pool)
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
    newTime <- fitRun bangVecs
    let score = (newTime / baseTime)
    putStrLn $ "bits: " ++ (concat $ (map (printBits . toBits) bangVecs))
    return $! Just score

  showGeneration _ (_,archive) = "best: " ++ (show fit)
    where
      (Just fit, _) = head archive
