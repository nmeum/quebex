module Golden (goldenTests) where

import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (IOMode (WriteMode), hClose, openFile)
import System.Process
import Test.Tasty
import Language.QBE (parse)
import Test.Tasty.Golden.Advanced

runQBE :: FilePath -> IO ExitCode
runQBE filePath = do
  devNull <- openFile "/dev/null" WriteMode

  (_, _, _, p) <-
    createProcess
      (proc "qbe" [filePath])
        { std_out = UseHandle devNull,
          std_err = UseHandle devNull
        }

  waitForProcess p <* hClose devNull

runQuebex :: FilePath -> IO ExitCode
runQuebex filePath = do
  content <- readFile filePath
  case parse filePath content of
    Right _ -> pure ExitSuccess
    Left _err -> do
      --fail $ show err
      pure $ ExitFailure 1

simpleCmp :: ExitCode -> ExitCode -> IO (Maybe String)
simpleCmp x y =
  return $
    if x == y
      then Nothing
      else Just ("Parsing mismatch: " ++ err)
  where
    err :: String
    err = "qbe=(" ++ show x ++ ") quebex=(" ++ show y ++ ")"

-- TODO: Could make {runQBE, runQuebex} return a pair which includes
-- the parser error message in order to output nicer erros in simpleCmp.
runTest :: TestName -> TestTree
runTest testName =
  goldenTest
    testName
    (runQBE fullPath)
    (runQuebex fullPath)
    simpleCmp
    (\_ -> pure ())
  where
    fullPath :: FilePath
    fullPath = "test" </> "golden" </> (testName ++ ".ssa")

------------------------------------------------------------------------

goldenTests :: TestTree
goldenTests =
  testGroup
    "goldenTests"
    [ runTest "data-definition-whitespace"]
