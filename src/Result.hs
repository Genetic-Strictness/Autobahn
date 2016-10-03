{-# LANGUAGE BangPatterns, OverloadedStrings, ExtendedDefaultRules #-}

module Result
where
import Rewrite
---
import qualified Data.Text.Lazy as L
import Text.Html
import Data.BitVector (BV, bitVec, toBits)
import GA
import System.Process
import System.Exit
import Data.List ((\\))
import Control.DeepSeq
import Debug.Trace

resultDir :: String
resultDir = "autobahn-results"

linkToCss :: String
linkToCss = "h1 {background-color: silver; text-align: center; margin-left: 35%; margin-right: 35%;}"
            ++ "body {background-color: white;}"
            ++ "p {text-indent: 1rem;}"
            ++ "div.center {height: 100%; display: flex; align-items: center; justify-content: center;}"
            ++ "div.left_img{overflow: auto;}"
            ++ "div.left_img ul{padding-left: 25%;}"
            ++ "table{border-collapse: collapse;}"
            ++ "td, th{border: 1px solid black; text-align: left; padding: 0.25em;}"

resultPageStyle :: Html
resultPageStyle = style $ toHtml linkToCss

programName :: FilePath -> Maybe String -> String
programName fp Nothing = fp
programName fp (Just name) = name

pageTitle :: String -> String
pageTitle name = "Autobahn: " ++ name ++ "'s results"

genResultPageHeader name = header $ titleTag +++ resultPageStyle
                              where
                              titleTag = thetitle $ toHtml $ pageTitle name

makeLink :: HTML a => a -> String -> Html
makeLink b ref = (anchor $ toHtml b) ! [href ref]

makeTableEntry :: HTML a => a -> Html
makeTableEntry a = td . strong $ toHtml a

makeTableRowLink :: (HTML a, HTML b) => (a,b) -> FilePath -> Html
makeTableRowLink (a,b) ref = tr $ makeTableEntry (makeLink a ref) +++ makeTableEntry b

makeTableRow :: (HTML a, HTML b) => (a,b) -> Html
makeTableRow (a,b) = tr $ makeTableEntry a +++ makeTableEntry b

makeTable :: (HTML a, HTML b) => [(a,b)] -> Html
makeTable as = table . concatHtml $ map makeTableRow as

tableHeader :: Html
tableHeader = tr $ (th $ toHtml "Chromosome") +++ (th $ toHtml "Performance")

genResultTableEntry :: (Floating a, Show a) => Int -> a -> FilePath -> Html
genResultTableEntry bv score fp = makeTableRowLink (show bv, show score) fp

genResultTable :: (Floating a, Show a) => [a] -> [FilePath] -> Html
genResultTable chroms fps = (p $ toHtml $ "Top " ++ n ++ " chromosomes: Higher is better") +++ resultTable
                           where
                           n = show $ length chroms
                           resultTable = table $ tableEntries
                           tableEntries = tableHeader +++ tableData
                           tableData = concatHtml $ tableDataList
                           tableDataList = map (uncurry $ uncurry genResultTableEntry) chroms'
                           chroms' = zip (zip [1..] chroms) fps

genParamTable :: GAConfig -> Float -> Int -> String -> Html
genParamTable cfg diversity fitnessRuns progName = (p $ toHtml $ "Autobahn run for " ++ progName ++ " with the following configuration") +++ paramTable
                                          where
                                          paramTable = table $ diversityEntry +++ generationEntry +++
                                                       popSizeEntry +++ archiveEntry +++ mutateRateEntry
                                                       +++ mutateProbEntry +++ crossoverRateEntry +++
                                                       fitnessEntry
                                          diversityEntry = makeTableRow ("diversityRate", show diversity)
                                          generationEntry = makeTableRow ("numGenerations", show $ getMaxGenerations cfg)
                                          popSizeEntry = makeTableRow ("populationSize", show $ getPopSize cfg)
                                          archiveEntry = makeTableRow ("archiveSize", show $ getArchiveSize cfg)
                                          mutateRateEntry = makeTableRow ("mutateRate", show $ getMutationRate cfg)
                                          mutateProbEntry = makeTableRow ("mutateProb", show $ getMutationParam cfg)
                                          crossoverRateEntry = makeTableRow ("crossRate", show $ getCrossoverRate cfg)
                                          fitnessEntry = makeTableRow ("numFitnessRuns", show fitnessRuns)


genResultPage :: (Floating a, Show a) => FilePath -> [a] -> [FilePath] -> FilePath -> Maybe String -> GAConfig -> Float -> Int -> IO ()
genResultPage projDir chroms fps fp name cfg diversity fitnessRuns = writeFile (projDir ++ "/" ++ resultDir ++ "/" ++ "result.html") $ htmlPage
                      where htmlPage = renderHtml $ headHtml +++ bodyHtml
                            bodyHtml = body $ pageTitleHtml +++ (genParamTable cfg diversity fitnessRuns progName) +++ results
                            results = genResultTable chroms fps
                            pageTitleHtml = h1 $ toHtml $ pageTitle progName
                            headHtml = genResultPageHeader progName
                            progName = programName fp name

createResultDir ::FilePath -> [FilePath] -> Int -> [BV] -> IO FilePath
createResultDir projDir srcs n bvs = do
      let newPath = projDir ++ "/" ++ resultDir ++ "/" ++ show n ++ "/"
      code <- system $ "mkdir -p " ++ newPath
      progs <- sequence $ map readFile srcs
      progs' <- sequence $ zipWith editBangs srcs (map toBits bvs)
      -- Recover the names of the source files
      let srcs' = map (\x -> newPath ++ x) $ map (\x -> x \\ projDir) srcs
      -- Write these new files into the new project directory
      rnf progs `seq` sequence $ zipWith writeFile srcs' progs'
      return newPath

createResultDirForAll :: FilePath -> [FilePath] -> [[BV]] -> IO [FilePath]
createResultDirForAll projDir srcs chroms = sequence $ map (uncurry createDirForProj) $ inputs
                                 where
                                 inputs = zip [1..] chroms
                                 createDirForProj = createResultDir projDir srcs
