{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GeneAlg2 where

import System.Random (mkStdGen, random, randoms, StdGen)
import GA (Entity(..), GAConfig(..), evolve, evolveVerbose, randomSearch, Archive)
import Data.BitVector (BV, fromBits, toBits, size, ones)
import Data.Bits.Bitwise (fromListLE, fromListBE, toListBE)
import Data.Bits
import Data.List
import Control.DeepSeq
import Config
import Types
--
-- GA TYPE CLASS IMPLEMENTATION
--

evo :: Cfg -> IO [([BangVec], Double)]
evo = undefined

ev = evolve :: StdGen -> GAConfig -> [BV] -> (Double, ([BV] -> IO (Double, Int))) -> IO (Archive [BV] Int)

-- TODO: implement in case we ever need to parse Bit (Bang) Vectors
readsBV = undefined

instance Read BV where -- TODO is this ok?
  readsPrec _ s = readsBV

printBits' :: [Bool] -> String
printBits' = concatMap (\b -> if b then "1" else "0")

instance Entity [BV] Int (Double, ([BV] -> IO (Double, Int))) [BV] IO where
 
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
                                  e' = e `xor` ((head pool) .&. fromBits bs)

  -- Improvement on base time
  -- NOTE: lower is better
  score (baseTime, fitRun) bangVecs = do 
    (newTime, nBangs) <- fitRun bangVecs
    let score = if newTime >= 0 then (newTime / baseTime) else 2
    putStrLn $ "bits: " ++ (concat $ (map (printBits' . toBits) bangVecs)) ++ ", " ++ (show nBangs) ++ ", " ++ (show score) ++ ", " ++ (show baseTime)
    return $! Just (if (score >= 0 && score <= 1.05) then nBangs else (1000))

  showGeneration _ (_,archive) = "best: " ++ (show fit)
    where
      (Just fit, _) = head archive
