{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Rewrite where

--
-- REWRITING HASKELL SRC
--

import System.FilePath
import Data.Data
import Data.Generics.Uniplate.DataOnly
import Language.Haskell.Exts
import Control.Monad
import Control.Monad.State.Strict
import Control.DeepSeq
import Control.Monad.Fail (MonadFail(..))
import Data.Functor.Identity (Identity(..))

findPats :: (Data (Pat SrcSpanInfo), Data a)=> a -> [Pat SrcSpanInfo]
findPats = universeBi

bangParseMode :: String -> ParseMode
bangParseMode path = defaultParseMode
  { parseFilename = path
  , baseLanguage = Haskell2010
  , extensions = [EnableExtension BangPatterns]
  , ignoreLanguagePragmas = False
  , ignoreLinePragmas = True
  , fixities = Just preludeFixities
  , ignoreFunctionArity = False
  }

placesToStrict :: String -> IO Int
placesToStrict path = do
  res <- parseFileWithMode (bangParseMode path) path
  case res of
    ParseFailed _ e -> error e
    ParseOk a       -> return $ length $ findPats a

readBangs :: String -> IO [Bool]
readBangs path = do
  res <- parseFileWithMode (bangParseMode path) path
  case res of
    ParseFailed _ e -> error e
    ParseOk a       -> return $ findBangs $ findPats a

findBangs :: [Pat SrcSpanInfo] -> [Bool]
findBangs = map isBang
  where isBang (PBangPat l p) = True
        isBang _ = False

-- Adds source locations in returned bit vector
readMinBangs :: FilePath -> IO (FilePath, [(Bool, Int)])
readMinBangs path = do
  res <- parseFileWithMode (bangParseMode path) path
  case res of
    ParseFailed _ e -> error e
    ParseOk a       -> return $ (takeFileName path, findMinBangs $ findPats a)

findMinBangs :: [Pat SrcSpanInfo] -> [(Bool, Int)]
findMinBangs = map isBang
  where isBang (PBangPat l p) = (True, startLine l)
        isBang _ = (False, -1)

editBangs :: String -> [Bool] -> IO String
editBangs path vec = do
  res <- parseFileWithMode (bangParseMode path) path
  case res of
    ParseFailed _ e -> error $ path ++ ": " ++ e
    ParseOk a       -> return $ prettyPrint $ stripTop $ fst $ changeBangs vec a

instance MonadFail Identity where
  fail = undefined

changeBangs :: [Bool] -> Module SrcSpanInfo -> (Module SrcSpanInfo, [Bool])
changeBangs bools x = runState (transformBiM go x) bools
  where go :: (Uniplate (Pat SrcSpanInfo)) => Pat SrcSpanInfo -> State [Bool] (Pat SrcSpanInfo)
        go pb@(PBangPat l p) = do
           (b:bs) <- get
           put bs
           if b
             then return pb
             else return p
        go pp = do
           (b:bs) <- get
           put bs
           if b
             then return (PParen (getLoc pp) (PBangPat (getLoc pp) pp))
             else return pp

stripTop :: Module SrcSpanInfo -> Module SrcSpanInfo
stripTop (Module a b c d decls) = Module a b c d (map rmBang decls)
    where rmBang (PatBind x (PParen loc (PBangPat l pb)) y z) = PatBind x pb y z
          rmBang x = x

getLoc :: Pat SrcSpanInfo -> SrcSpanInfo
getLoc (PVar l _ ) = l
getLoc (PLit l _ _ ) = l
getLoc (PNPlusK l _ _ ) = l
getLoc (PInfixApp l _ _ _ ) = l
getLoc (PApp l _ _ ) = l
getLoc (PTuple l _ _ ) = l
getLoc (PList l _ ) = l
getLoc (PParen l _ ) = l
getLoc (PRec l _ _ ) = l
getLoc (PAsPat l _ _ ) = l
getLoc (PWildCard l) = l
getLoc (PIrrPat l _) = l
getLoc (PatTypeSig l _ _ ) = l
getLoc (PViewPat l _ _ ) = l
getLoc (PRPat l _ ) = l
getLoc (PXTag l _ _ _ _ ) = l
getLoc (PXETag l _ _ _ ) = l
getLoc (PXPcdata l _ ) = l
getLoc (PXPatTag l _ ) = l
getLoc (PXRPats  l _ ) = l
getLoc (PQuasiQuote l _ _ ) = l
getLoc (PBangPat l _ ) = l
