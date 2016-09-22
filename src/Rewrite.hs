{-# LANGUAGE BangPatterns #-}

module Rewrite where

--
-- REWRITING HASKELL SRC
--

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts
import Control.Monad
import Control.Monad.State.Strict
import Control.DeepSeq

findPats :: Data a => a -> [Pat]
findPats = universeBi

placesToStrict :: String -> IO Int
placesToStrict path = do
  content <- readFile path
  let mode = ParseMode path Haskell2010 [EnableExtension BangPatterns] False False Nothing
  case parseModuleWithMode mode content of
    ParseFailed _ e -> error e
    ParseOk a       -> rnf content `seq` return $ length $ findPats a


readBangs :: String -> IO [Bool]
readBangs path = do
  content <- readFile path
  let mode = ParseMode path Haskell2010 [EnableExtension BangPatterns] False False Nothing
  case parseModuleWithMode mode content of
    ParseFailed _ e -> error e
    ParseOk a       -> rnf content `seq` return $ findBangs $ findPats a

findBangs :: [Pat] -> [Bool]
findBangs = map isBang
  where isBang (PBangPat p) = True
        isBang _ = False

editBangs :: String -> [Bool] -> IO String
editBangs path vec = do
  content <- readFile path
  let mode = ParseMode path Haskell2010 [EnableExtension BangPatterns] False False Nothing
  case parseModuleWithMode mode content of
    ParseFailed _ e -> error $ path ++ ": " ++ e
    ParseOk a       -> rnf content `seq` return $ prettyPrint $ stripTop $ fst $ changeBangs vec a

changeBangs :: [Bool] -> Module -> (Module, [Bool])
changeBangs bools x = runState (transformBiM go x) bools
  where go pb@(PBangPat p) = do
           (b:bs) <- get
           put bs
           if b
             then return pb
             else return p
        go pp = do
           (b:bs) <- get
           put bs
           if b
             then return (PParen (PBangPat pp))
             else return pp

stripTop :: Module -> Module
stripTop (Module a b c d e f decls) = Module a b c d e f (map rmBang decls)
    where rmBang (PatBind x (PParen (PBangPat pb)) y z) = PatBind x pb y z
          rmBang x = x
