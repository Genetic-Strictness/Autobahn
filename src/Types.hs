module Types
  ( MetricType(..)
  , AlgorithmTy(..)
  , BangVec(..)
  , Time(..)
  , CCSrc(..)
  , Cfg(..)
  , CfgAST(..)
  , defaultCfg
  , defaultCfgWithProjectDir
  , Int64
  , defaultProjDir
  , defaultTimeLimit
  , defaultCoverage
  ) where
import Data.Int (Int64)
import Data.BitVector (BV, fromBits, toBits, size, ones)

data MetricType = ALLOC | GC | RUNTIME

instance Show MetricType where
    show ALLOC = "alloc"
    show GC = "gc"
    show RUNTIME = "runtime"

data AlgorithmTy = ORIGINAL | MINIMIZE

instance Show AlgorithmTy where
    show ORIGINAL = "original"
    show MINIMIZE = "minimize"

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
  , algorithm :: AlgorithmTy
  , absenceImpact :: Double
  , hotSpotThresh :: Double
  , profileMetric :: String } deriving Show

data CfgAST = 
    BUDGET Double
  | DIR String
  | SRCS [String]
  | METRIC MetricType
  | INPUTS [String]
  | FITRUNS Integer
  | FILE [CfgAST]
  | EXE String
  | ALG AlgorithmTy
  | PROFILE String 
  deriving Show

type BangVec = BV 
type Time = Double
type CCSrc = [([Char], (Int, Int))]

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

defaultProfile :: String 
defaultProfile = "RT" 

defaultInput :: String
defaultInput = ""

defaultFitRuns :: Integer
defaultFitRuns = toInteger 1

defaultHotSpotThresh :: Double
defaultHotSpotThresh = 0.05

defaultAbsenceImpact :: Double
defaultAbsenceImpact = 0.06

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
  , algorithm = ORIGINAL
  , absenceImpact = defaultAbsenceImpact
  , hotSpotThresh = defaultHotSpotThresh
  , profileMetric = defaultProfile
  }

defaultCfgWithProjectDir a = Cfg
  { projectDir = a
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
  , algorithm = ORIGINAL
  , absenceImpact = defaultAbsenceImpact
  , hotSpotThresh = defaultHotSpotThresh
  , profileMetric = defaultProfile
  }
