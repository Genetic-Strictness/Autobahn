module Types
  ( MetricType(..)
  , Cfg(..)
  , CfgAST(..)
  , defaultCfg
  , Int64
  , defaultProjDir
  , defaultTimeLimit
  , defaultCoverage
  ) where
import Data.Int (Int64)

data MetricType = ALLOC | GC | RUNTIME

instance Show MetricType where
    show ALLOC = "alloc"
    show GC = "gc"
    show RUNTIME = "runtime"

data Cfg = Cfg 
  { projectDir :: String
  , timeBudget :: Double
  , getBaseTime :: Double
  , coverage :: [String]
  , executable :: String
  , fitnessMetric :: MetricType
  , getBaseMetric :: Double
  , pop :: Int
  , gen :: Int
  , arch :: Int
  , fitnessRuns :: Int64
  , inputArgs :: String
  } deriving Show

data CfgAST = 
    BUDGET Double
  | DIR String
  | SRCS [String]
  | METRIC MetricType
  | INPUTS [String]
  | FITRUNS Integer
  | FILE [CfgAST]
  | EXE String
  deriving Show

defaultProjDir :: FilePath
defaultProjDir = "."

defaultTimeLimit :: Double
defaultTimeLimit = 3

defaultTimeLimitSec :: Double
defaultTimeLimitSec = defaultTimeLimit * 60 * 60

defaultCoverage :: String
defaultCoverage = "Main.hs"

defaultExecutable :: String
defaultExecutable = "Main.hs"

defaultMetric :: MetricType
defaultMetric = RUNTIME

defaultInput :: String
defaultInput = ""

defaultFitRuns :: Integer
defaultFitRuns = toInteger 1

defaultCfg = Cfg
  { projectDir = defaultProjDir
  , executable = defaultExecutable
  , timeBudget = defaultTimeLimitSec
  , getBaseTime = 0.0 - 1.0
  , coverage = words defaultCoverage
  , fitnessMetric = defaultMetric
  , getBaseMetric = 0.0 - 1.0
  , pop = 1
  , gen = 1
  , arch = 1
  , fitnessRuns = fromIntegral defaultFitRuns
  , inputArgs = defaultInput
  }

