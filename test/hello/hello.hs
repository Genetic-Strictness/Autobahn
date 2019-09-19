{-# LANGUAGE BangPatterns #-}
module Main (main, fib) where
import System.Environment

fib 0 = 1
fib 1 = 1
fib (!n) = (+) (fib $ n - 1) (fib $ n - 2)



main = do
  as <- getArgs
  let a = read $ head as :: Int
  putStrLn $ show a
  putStrLn $ show $ fib a

