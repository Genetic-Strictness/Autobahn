module Main (main) where
import System.Environment
import Control.Monad
import System.Mem
import Data.Bits
import Text.Printf
import GHC.Conc

data Tree = Nil
          | Node !Int !Tree !Tree
minN = 4
io ((!s)) n (!t) = printf "%s of depth %d\t check: %d\n" s n t
main
  = do let n = 20
       let (!maxN) = max (minN + 2) n
           (!(!(!stretchN))) = maxN + 1
       let (!c) = {-# SCC "stretch" #-} check (make 0 stretchN)
       io "stretch tree" stretchN c
       let (!(!(long))) = make 0 maxN
       let (!vs) = depth minN maxN
       mapM_ (\ ((!((m, (d), ((!i)))))) -> io (show m ++ "\t trees") d i)
         vs
       io "long lived tree" maxN (check long)

depth :: Int -> Int -> [(Int, Int, Int)]
depth ((!d)) m
  | d <= m =
    let s = sumT d n 0
        (!(!(!rest))) = depth (d + 2) m
      in s `par` ((2 * n, d, s) : rest)
  | otherwise = []
  where (!(!(!(n)))) = bit (m - d + minN)

sumT :: Int -> Int -> Int -> Int
sumT (!((!d))) 0 ((!t)) = t
sumT (d) (!i) (!t) = a `par` b `par` sumT d (i - 1) ans
  where ((!(!a))) = check (make i d)
        (!(!(!b))) = check (make (-i) d)
        ans = a + b + t
check = check' True 0

check' :: Bool -> Int -> Tree -> Int
check' (!(!(!b))) (!((z))) (!Nil) = z
check' (!(!b)) ((!(!z))) ((Node i (!((!l))) ((!r))))
  = check' (not b) (check' b (if b then z + i else z - i) l) r

make :: Int -> Int -> Tree
make (!(!((!i)))) 0 = Node i Nil Nil
make (!((!(i)))) (!((!d))) = Node i (make (i2 - 1) d2) (make i2 d2)
  where i2 = 2 * i
        (!d2) = d - 1