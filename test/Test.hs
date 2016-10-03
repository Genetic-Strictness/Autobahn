module Main where
import System.Process (system)
import System.Exit (ExitCode(..))
import System.Directory
    ( setCurrentDirectory
    , getCurrentDirectory
    , doesDirectoryExist
    , doesFileExist
    )
import Control.Monad (liftM)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

run x = do
  putStrLn $ "Running: " ++ x
  system x

autobahnMe :: FilePath -> [FilePath] -> IO ExitCode
autobahnMe projDir srcNames = do
  pwd <- getCurrentDirectory
  setCurrentDirectory projDir
  -- Assume the worst - Autobahn was already run and we clobbered the source file:
  mapM (\s -> run $ "git checkout " ++ s) srcNames
  run "rm -rf autobahn-results autobahn-survivor dist"
  {- Nesting of calls to stack / cabal doesn't seem to play
     nicely - need to unset GHC_PACKAGE_PATH as
     per https://github.com/haskell/cabal/issues/1944 -}
  rc <- run "unset GHC_PACKAGE_PATH && Autobahn"
  setCurrentDirectory pwd
  return rc

dirExists = doesDirectoryExist
fileExists = doesFileExist

helloFib = do
  let base = "test/hello/"
  rc <- autobahnMe base ["hello.hs"]
  assert $ rc == ExitSuccess
  let results  = base ++ "autobahn-results"
  let survivor = base ++ "autobahn-survivor"

  assert $ dirExists survivor
  assert $ dirExists $ results ++ "/1"
  assert $ fileExists $ survivor ++ "/hello.hs"
  assert $ fileExists $ results ++ "/result.html"
  assert $ fileExists $ results ++ "/1/hello.hs"
  assert $ liftM not $ fileExists $ base ++ "/timing.temp"

main :: IO ()
main = defaultMainWithOpts
  [ testCase "hello" helloFib
  ] mempty


