{-# LANGUAGE BangPatterns #-}

import Types
import Rewrite
import Profiling
import qualified GeneAlg as G1
import qualified GeneAlg2 as G2
import Config
import Result
import Parse
import Min
------
import GA
import qualified Data.BitVector as B
import Data.Bits
import Data.Int
import System.Environment
import System.IO
import System.Process
import System.Directory hiding (executable)
import Text.Read
import Control.DeepSeq
import System.FilePath

import Debug.Trace
import System.Random (StdGen, getStdGen)
import qualified Data.Maybe as D

import Data.List
import Utils

reps :: Int64
reps = runs

cfgFile :: FilePath
cfgFile = "config.atb"

performEvolve :: Cfg -> IO [([BangVec], Double)]
performEvolve cfg = let alg = algorithm cfg in
                    case alg of
                      ORIGINAL -> G1.ev cfg
                      otherwise -> error $ "Unkown algorithm type: " ++ (show alg)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  cfg <- if null args then gatherCfg else manufactureCfg args
  putStrLn "Setting up optimization process..."
  putStrLn "Starting optimization process..."
  gmain cfg
  putStrLn $ "Optimization finished, please inspect and select candidate changes "
   ++ "(found in `autobah-results/` directory under project root)"
      where
          gatherCfg = do
            putStrLn "Configure optimization..."
            cfgExist <- doesFileExist cfgFile
            cfg <- if cfgExist then readCfg cfgFile else cliCfg
            return cfg
          manufactureCfg args = do
                                     [a, b, c, d, e, f, g] <- getArgs
                                     (baseTime, baseMetric) <- benchmark (defaultCfgWithProjectDir a defaultExecutable) 1
                                     allFiles <- getDirectoryContents a
                                     print (baseTime, baseMetric)
                                     return Cfg { projectDir = a
                                                , timeBudget = deriveFitnessTimeLimit baseTime
                                                , getBaseTime = baseTime
                                                , coverage = filter (isSuffixOf ".hs") allFiles
                                                , executable = ""
                                                , fitnessMetric = RUNTIME
                                                , getBaseMetric = baseMetric
                                                , pop = (read b)
                                                , gen = (read c)
                                                , arch = (read d)
                                                , fitnessRuns = 1
                                                , inputArgs = ""
                                                , algorithm = ORIGINAL
                                                , absenceImpact = (read e)
                                                , hotSpotThresh = (read f)
                                                , profileMetric = g
                                                }

mkCfg projDir exe inpArgs =
  (defaultCfgWithProjectDir projDir exe)
    { inputArgs = inpArgs
    }

gmain :: Cfg -> IO ()
gmain autobahnCfg = do
    let projDir = projectDir autobahnCfg
        exe = executable autobahnCfg
        cfg = createGAConfig autobahnCfg
        filesInDir = coverage autobahnCfg
        fitnessReps = fitnessRuns autobahnCfg
        baseTime  = getBaseTime autobahnCfg
        baseMetric  = getBaseMetric autobahnCfg
        inpArgs = inputArgs autobahnCfg
    putStrLn $ "Optimizing " ++ projDir
    putStrLn $ ">>>>>>>>>>>>>>>START OPTIMIZATION>>>>>>>>>>>>>>>"
    putStrLn $ "pop: " ++ (show $ pop autobahnCfg)
    putStrLn $ "gens: " ++ (show $ gen autobahnCfg)
    putStrLn $ "arch: " ++ (show $ arch autobahnCfg)
    putStrLn $ "profile metric: " ++ (show $ profileMetric autobahnCfg)
    putStrLn $ "absenceImpact: " ++ (show $ absenceImpact autobahnCfg)
    putStrLn $ "hotSpotThresh: " ++ (show $ hotSpotThresh autobahnCfg)

-- If basetime too short or error, don't optimize
    checkBaseProgram baseTime baseMetric

--------------------Pre-profiling-----------------------
-- Identify files to optimize.
-- If no files suitable for optimization, error.
-- If files to optimize not in directory, alert user.

    putStrLn $ ">>>>>>>>>>>>>>>PRE-OPTIMIZATION>>>>>>>>>>>>>>>"

    benchmark (mkCfg projDir exe inpArgs) 1
    hs <- parseProfile (profileMetric autobahnCfg) (hotSpotThresh autobahnCfg)
            -- $ addExtension (takeBaseName projDir) "prof"
            $ addExtension (projDir ++ "/" ++ (takeBaseName exe)) "prof"

    let hs'     = compileFiles hs
        hsFiles = fst $ unzip hs'

    extraFiles <- checkFiles hsFiles filesInDir

    let files      = hsFiles \\ extraFiles
        noCutFiles = (length filesInDir) - (length files)
        cutFiles   = filesInDir \\ files

    let cutFiles'         = map (\x -> projDir ++ "/" ++ x) cutFiles
    cutPs <- sequence $ map readBangs cutFiles'

    putStrLn "Original files in directory:"
    print filesInDir
    putStrLn "Files cut during pre-opt:"
    print cutFiles

    let noCutPats = foldl (\acc x -> acc + length x) 0 cutPs
        resPre    = "noExtraFiles\t" ++ (show $ length extraFiles) ++ "\textraFiles\t"
                    ++ (show extraFiles) ++ "\tnoCutFiles\t" ++ (show noCutFiles)
                    ++ "\tnoCutPats\t" ++ (show noCutPats)

--------------------Autobahn-----------------------
    putStrLn $ ">>>>>>>>>>>>>>>AUTOBAHN 1.0>>>>>>>>>>>>>>>"

    let absPaths         = map (\x -> projDir ++ "/" ++ x) files
        fitnessTimeLimit = deriveFitnessTimeLimit baseTime

-- Pool: bit vector representing original progam
    let paths = map (\x -> projDir ++ "/" ++ x) filesInDir
    progs <- sequence $ map readFile absPaths
    bs    <- sequence $ map readBangs absPaths
    let !vecPool = rnf progs `seq` map B.fromBits bs
        len      = numPats bs

-- Do the evolution!
    generator <- getStdGen -- get random generator each time
    es        <- G1.evolveProg generator cfg vecPool (baseMetric,
                                       fitness (autobahnCfg { getBaseTime = fitnessTimeLimit }) fitnessReps files)

    let e         = snd $ head es :: [BangVec]
        score     = case (fst $ head es) of
                        Just s -> s
                        Nothing -> (2.0, []) :: G1.Score
        atbResult = if (fst score) > 1 then vecPool else e
        bitVec    = map B.toBits e

    progs' <- sequence $ map (uncurry editBangs) $ zip absPaths bitVec

    let bestMetricPerc = fst $ D.fromJust $ fst $ head $ filter (\x -> D.isJust $ fst x) es
    checkImprovement (bestMetricPerc*baseTime) baseTime

    let origNumBangs   = countBangs bs
        autNumBangs    = countBangs bitVec
        resAut         = "baseTime\t1" ++ "\torigNumBangs\t" ++ (show origNumBangs)
                         ++ "\tAut1Time\t" ++ (show bestMetricPerc) ++ "\tAut1Bangs\t"
                         ++ (show autNumBangs)

-- Write the original files back to disk
    sequence $ map (uncurry writeFile) $ zip absPaths progs

-- Debug: Print out actual ATB and Minimizer results
    putStrLn $ "ATB Result: " ++ (unlines $ (map (G1.printBits . B.toBits) e))

-- Write result
    putStrLn $ "best entity (GA): " ++ (unlines $ (map (G1.printBits . B.toBits) atbResult))

    let newPath = projDir ++ "/" ++ "autobahn-survivor"
    code <- system $ "mkdir -p " ++ newPath

    let surPaths     = map (\x -> projDir ++ "/" ++ "autobahn-survivor/" ++ x) files
        survivorDirs = map (takeDirectory) surPaths
    sequence $ map (\x -> system $ "mkdir -p " ++ x) survivorDirs
    sequence $ map (uncurry writeFile) $ zip surPaths progs'

-- Write result page
    let es'   = filter (\x -> fst x /= Nothing) es
        bangs = (map snd es') :: [[BangVec]]
    newFps <- createResultDirForAll projDir absPaths bangs
    let f          = map fst es'
        getScore s = case s of
                        Nothing -> error "filter should have removed all Nothing"
                        Just n -> n
        scores     = map getScore f
        help a     = a
    genResultPage projDir (map fst scores) newFps projDir Nothing cfg 0.0 1

--------------------Min-----------------------
    putStrLn $ ">>>>>>>>>>>>>>>POST-OPTIMIZATION>>>>>>>>>>>>>>>"

-- obtain original profile
-- Pool: bit/loc vector representing original progam
-- need to manually erase bangs from surPaths because original file
-- contains extra lines/comments
    bsA      <- sequence $ map readMinBangs surPaths
    let bsUnopt = map (\(f, bangs) -> (f, eraseBangs bangs)) bsA
    unopt_progs <- sequence $ map (uncurry editBangs) $ zip surPaths (map (\x -> fst (unzip (snd x))) bsUnopt)
    sequence $ map (uncurry writeFile) $ zip absPaths unopt_progs

    benchmark (mkCfg projDir exe inpArgs) 1
    hotSpots <- parseProfile (profileMetric autobahnCfg) (hotSpotThresh autobahnCfg)
--                  $ addExtension (takeBaseName projDir) "prof"
                  $ addExtension (projDir ++ "/" ++ (takeBaseName exe)) "prof"

    let hotSpots'     = compileFiles hotSpots

-- Pool: bit/loc vector representing optimized progam
--  copy survivor files, get survivor base time
    progsA <- sequence $ map readFile surPaths
    sequence $ map (uncurry writeFile) $ zip absPaths progsA
    bsA      <- sequence $ map readMinBangs absPaths
    (surTime, surMetric) <- benchmark (mkCfg projDir exe inpArgs) 1
    print ("SUR TIME IS " ++ (show surTime))
    checkImprovement surTime baseTime

-- individually test each hot spot
    let bsHS = modBangs hotSpots' bsA
    minBangs <- mapM (\src -> do
      let bs'' = modBangs (hotSpots' \\ [src]) bsHS
      progs'' <- sequence $ map (uncurry editBangs) $ zip surPaths (map (\x -> fst $ unzip $ snd x) bs'')
      sequence $ map (uncurry writeFile) $ zip absPaths progs''
      (tempTime, tempBaseMetric) <- benchmark (mkCfg projDir exe inpArgs) 1
      if (((tempTime - surTime)/surTime) > (absenceImpact autobahnCfg)) then return [src] else return []) hotSpots'

    let minBangs'    = concat minBangs
        bs'          = modBangs minBangs' bsA
        minNumBangs  = foldl (\acc (f, b) -> acc + (numBangs b)) 0 bs'
        surNumBangs  = foldl (\acc (f, b) -> acc + (numBangs b)) 0 bsA
    min_progs <- sequence $ map (uncurry editBangs) $ zip surPaths (map (\x -> fst $ unzip $ snd x) bs')
    sequence $ map (uncurry writeFile) $ zip absPaths min_progs
    (minTime, minMetric) <- benchmark (mkCfg projDir exe inpArgs) 1
    checkRunTimeError minTime

    putStrLn ">>>>>>>>>>>>>>FINISH MIN>>>>>>>>>>>>>>>"

    let timeRatio = minTime/surTime
        bangRatio = (fromIntegral minNumBangs)/(fromIntegral surNumBangs)
        resPost   = "Aut2:\toriginalTime 1\torigBangs\t" ++ (show origNumBangs)
                     ++ "\tAut1Time\t" ++ (show (surTime/baseTime)) ++ "\tAut1Bangs\t" ++ (show surNumBangs)
                     ++ "\tAut2Time\t" ++ (show (minTime/baseTime)) ++ "\tAut2Bangs\t" ++ (show minNumBangs)
    writeFile "minresults" (resPre ++ "\n" ++ resAut ++ "\n" ++ resPost)
