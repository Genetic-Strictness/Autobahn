module Config where

import GA
import System.Random
import Data.Int
import Profiling

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.String as PS
import qualified Text.Parsec.Prim as PP
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Expr as PE
import qualified Text.Parsec.Combinator as PC
import Text.ParserCombinators.Parsec.Language (haskellStyle, reservedOpNames, reservedNames)
import Text.ParserCombinators.Parsec.Pos (newPos)
import Text.Read

import Debug.Trace
--
-- CONFIG FOR FITNESS RUN
--

runs :: Int64
runs = 1

-- 
-- CONFIG FOR GENETIC ALG
-- 

crossRate  = 0.6 :: Float
muteRate   = 0.4 :: Float
crossParam = 0.0 :: Float
muteParam  = 0.3 :: Float
checkpoint = False :: Bool
rescoreArc = False :: Bool

{-
cfg = GAConfig 
        15 -- population size
        1 -- archive size (best entities to keep track of)
        5 -- maximum number of generations
        0.8 -- crossover rate (% of entities by crossover)
        0.2 -- mutation rate (% of entities by mutation)
        0.0 -- parameter for crossover (not used here)
        0.2 -- parameter for mutation (% of flipped letters)
        False -- whether or not to use checkpointing
        False -- don't rescore archive in each generation
-}

g = mkStdGen 0 -- random generator

data Cfg = Cfg { projectDir :: String
               , timeBudget :: Double
               , getBaseTime :: Double
               , coverage :: [String]
               , fitnessMetric :: MetricType
               , getBaseMetric :: Double
               , pop :: Int
               , gen :: Int
               , arch :: Int
               , fitnessRuns :: Int64
               , inputArgs :: String
{-             , diverRate :: Float
               , mutRate :: Float
               , xRate :: Float -}
               } deriving Show

defaultProjDir :: FilePath
defaultProjDir = "."

defaultTimeLimit :: Double
defaultTimeLimit = 3

defaultTimeLimitSec :: Double
defaultTimeLimitSec = defaultTimeLimit * 60 * 60

defaultCoverage :: String
defaultCoverage = "Main.hs"

defaultMetric :: MetricType
defaultMetric = RUNTIME

defaultInput :: String
defaultInput = ""

defaultFitRuns :: Integer
defaultFitRuns = toInteger 1

emptyCfg :: Cfg
emptyCfg = Cfg defaultProjDir defaultTimeLimitSec (0-1) (words defaultCoverage) defaultMetric (0-1) 1 1 1 (fromInteger defaultFitRuns) defaultInput

deriveFitnessTimeLimit :: Double -> Double
deriveFitnessTimeLimit = (*) 2

readLnWDefault :: Read a => a -> IO a
readLnWDefault def = do
  cont <- getLine
  case readMaybe cont 
    of Nothing -> return def
       Just res -> return res

{-
 - Create the configuration specific to the GA library
 -}
createGAConfig :: Cfg -> GAConfig
createGAConfig cfg = GAConfig pop' arch' gens' xRate mRate xParam mParam chkpt rescore
                     where
                       pop' = pop cfg
                       gens' = gen cfg
                       arch' = arch cfg
                       xRate = crossRate
                       mRate = muteRate
                       xParam = crossParam
                       mParam = muteParam
                       chkpt = checkpoint
                       rescore = rescoreArc
{-
 -  Read from the command line and produce an Autobahn Configuration
 -}
cliCfg :: IO Cfg
cliCfg = do 
  putStrLn "No config.atb file found, please specify parameters as prompted"
  putStrLn "<Enter> to use [defaults]"

  putStr "Path to project program sources [\".\"]:"
  projDir <- readLnWDefault $ (show defaultProjDir)

  putStr "Time alloted for Autobahn [3h]:"
  timeLimit <- readLnWDefault $ (show defaultTimeLimit) ++ "h"

  putStr "File(s) to add/remove bangs in [\"Main.hs\"]:"
  srcs <- readLnWDefault defaultCoverage

  putStr "Performance metric to optimize [\"runtime\"]:"
  metric <- readLnWDefault "runtime"

  putStr "Representative input data & arguments [no input/arguments]:"
  args <- readLnWDefault ""

  putStr "Times to run program for fitness measurement [1]:"
  nRuns <- readLnWDefault "1"

  -- Now we take their answers and produce a configuration file
  cliCfgFile <- return $ unlines $ [ ("projectDirectory = " ++ projDir)
                               , ("budgetTime = " ++ timeLimit)
                               , ("coverage = " ++ srcs)
                               , ("targetMetric = " ++ metric)
                               , ("inputArg = " ++ args)
                               , ("fitnessRuns = " ++ nRuns)]
  -- That we now parse that file
  result <- return $ parseCfgFile "cli" 1 1 cliCfgFile
  case result of
       Left err -> error $ show err
       Right ast -> calculateInputs $ convertToCfg ast emptyCfg

readCfg :: FilePath -> IO Cfg
readCfg fp = do {
          text <- readFile fp
          ; x <- return $ parseCfgFile fp 1 1 text
          ; case x of
                Left err -> error $ show err
                Right ast -> calculateInputs $ convertToCfg ast emptyCfg
          }

{-
 - Determine the configuration from the time limit and the base time
 - Derived from 
 -    (1) the ratio of generations to population, 4 : 3, from the paper and
 -    (2) generations * population * (2 * baseTime) = timeLimit
 -}
heuristic :: String -> Double -> Double -> Int64 -> (Int, Int, Int)
heuristic projDir baseTime timeLimit nRuns = ((round pop), (round gen), (round arch))
                                       where
                                       fitnessTimeLimit = (fromInteger . toInteger $ nRuns) * deriveFitnessTimeLimit baseTime
                                       n = (timeLimit / fitnessTimeLimit) :: Double
                                       pop = (sqrt $ (3 * n)/4) :: Double
                                       gen = (4 * pop)/3 :: Double
                                       arch = if (round $ pop/2) <= 0 then 1 else pop/2

calculateFitRuns :: String -> String -> Double -> MetricType -> (Double, Double) -> Int64 -> IO (Double, Double, Int64)
calculateFitRuns projDir args timeLimit metric (accTime, accMetric) n = do
                                            (baseTime, baseMetric) <- benchmark projDir args (timeLimit) metric (1 :: Int64)
                                            let (accTime', accMetric') = (baseTime + accTime, baseMetric + accMetric)
                                                (newTimeMean, newMetricMean)= (accTime' / (fromInteger . toInteger $ n+1), accMetric' / (fromInteger . toInteger $ n+1))
                                                percChange = abs $ (currentTimeMean - newTimeMean)/currentTimeMean
                                            if baseTime < 0 then error "Program did not run" else print newTimeMean
                                            if percChange <= 0.05
                                              then return $ (newTimeMean, newMetricMean, n)
                                              else calculateFitRuns projDir args timeLimit metric (accTime', accMetric') (n + 1)
                                            where
                                               currentTimeMean = accTime / (fromInteger . toInteger $ n)

{-
 - Convert the time budget to a number of generations,
 - population size, and archive/selection size.
 -}
convertTimeToGens :: String -> String -> Double -> MetricType -> IO (Int, Int, Int, Int64, Double, Double)
convertTimeToGens projDir args timeLimit metric = do
                          buildProj projDir
                          (baseTime, baseMetric) <- benchmark projDir args (timeLimit) metric 1
                          (meanTime, meanMetric, nRuns) <- calculateFitRuns projDir args (timeLimit) metric (baseTime, baseMetric) 1
                          -- Remove the number of program runs per chromosome from time limit
                          n <- return . fromInteger . toInteger $ runs :: IO Double
                          timeLimit' <- (return $ timeLimit / n)
                          (pop, gen, arch) <- return $ heuristic projDir meanTime timeLimit' nRuns
                          return $ (pop, gen, arch, nRuns, meanTime, meanMetric)
data CfgAST = BUDGET Double
            | DIR String
            | SRCS [String]
            | METRIC MetricType
            | INPUTS [String]
            | FITRUNS Integer
            | FILE [CfgAST] deriving Show

hoursToSeconds :: Double -> Double
hoursToSeconds = (*) $ 60 * 60

convertToCfg :: [CfgAST] -> Cfg -> Cfg
convertToCfg []                   cfg = cfg
convertToCfg ((DIR path) : ast)   cfg = convertToCfg ast $ cfg { projectDir = path }
convertToCfg ((BUDGET n) : ast)   cfg = let timeInSeconds = hoursToSeconds $ n
                                        in convertToCfg ast $ cfg { timeBudget = timeInSeconds }
convertToCfg ((SRCS srcs) : ast) cfg = convertToCfg ast $ cfg { coverage = srcs }
convertToCfg ((FILE inner) : ast) cfg = convertToCfg ast $ convertToCfg inner cfg
convertToCfg ((INPUTS args) : ast) cfg = convertToCfg ast $ cfg { inputArgs = concat args }
-- Ignore the other AST Nodes
convertToCfg ((_) : ast)          cfg = convertToCfg ast cfg

calculateInputs :: Cfg -> IO Cfg
calculateInputs cfg = do
            metric <- return $ fitnessMetric cfg
            timeLimit <- return $ timeBudget cfg
            let projDir = projectDir cfg
                args = inputArgs cfg
            (pop', gen', arch', nRuns, baseTime, baseMetric) <- convertTimeToGens projDir args timeLimit metric
            return $ cfg { getBaseTime = baseTime
                         , getBaseMetric = baseMetric
                         , pop = pop'
                         , gen = gen'
                         , arch = arch'
                         , fitnessRuns = nRuns
                         }

parseCfgFile :: SourceName -> Line -> Column -> String -> Either ParseError [CfgAST]
parseCfgFile fileName ln col text = 
  PP.parse cfgDefs fileName text
  where
    cfgDefs = do {
      setPosition (newPos fileName ln col)
    ; whiteSpace
    ; x <- configAllOptions
    ; whiteSpace
    ; eof <|> errorParse
    ; return x
    }

    -- Eats remaining tokens and raises unexpected error
    errorParse = do {
      rest <- manyTill anyToken eof
    ; unexpected rest
    }

-- Thanks to Matthew Ahrens for the Parsec boilerplate
-- Thanks to Karl Cronburg for the help with Parsec rules

----------- Parser for Config File  --------------

{-
 - Allows the <|> to backtrack if one case fails
 - Not the most performant, but it works for our purposes.
 -}
(<||>) a b = try a <|> try b

configAllOptions :: PS.Parser [CfgAST]
configAllOptions = many configOption

configOption :: PS.Parser CfgAST
configOption = do {
               whiteSpace
               ; x <- configTopLevel
               ; whiteSpace
               ; return x
               }

configTopLevel :: PS.Parser CfgAST
configTopLevel = projDirRule <||> budgetRule <||> coverageRule
               <||> targetMetricRule <||> inputArgRule <||> fitnessRunRule

budgetRule :: PS.Parser CfgAST
budgetRule = do {
             reserved "budgetTime"
             ; reservedOp "="
             ; x <- naturalOrFloat
             ; symbol "h"
             ; return $ case x of
                           Left n -> BUDGET $ fromInteger n
                           Right n -> BUDGET n
             }

coverageRule :: PS.Parser CfgAST
coverageRule = do {
               reserved "coverage"
               ; reservedOp "="
               ; xs <- stringLiteral `sepBy` (symbol ",")
               ; return $ SRCS xs
               }

projDirRule :: PS.Parser CfgAST
projDirRule = do {
               reserved "projectDirectory"
               ; reservedOp "="
               ; fp <- stringLiteral
               ; return $ DIR fp
               }

targetMetricRule :: PS.Parser CfgAST
targetMetricRule = do {
                   reserved "targetMetric"
                   ; reservedOp "=" 
                   ; x <- parseMetric
                   ; return $ METRIC x
                   }

parseMetric :: PS.Parser MetricType
parseMetric = do {
              x <- stringLiteral
              ; case x of
                  "peakAlloc" -> return $ ALLOC
                  "runtime"   -> return $ RUNTIME
                  "gc"        -> return $ GC
              }

inputArgRule :: PS.Parser CfgAST
inputArgRule = do {
               reserved "inputArg"
               ; reservedOp "="
               ; xs <- many stringLiteral
               ; return $ INPUTS xs
               }

fitnessRunRule :: PS.Parser CfgAST
fitnessRunRule = do {
                 reserved "fitnessRuns"
                 ; reservedOp "="
                 ; x <- natural
                 ; return $ FITRUNS x
                 }

---- Lexer ----

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { reservedOpNames = ["="]
    , reservedNames = ["budgetTime", "confidence", "coverage",
                       "targetMetric", "inputArg", "fitnessRuns",
                       "projectDirectory"]
  }

whiteSpace = PT.whiteSpace  lexer
identifier = PT.identifier lexer
charLiteral = PT.charLiteral lexer
stringLiteral = PT.stringLiteral lexer
naturalOrFloat = PT.naturalOrFloat lexer
natural = PT.natural lexer
reserved = PT.reserved lexer
reservedOp = PT.reservedOp lexer
symbol = PT.symbol lexer


