module Golden (goldenTests) where

import Language.QBE (parse)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (IOMode (WriteMode), hClose, openFile, hGetContents)
import System.Process
import Test.Tasty
import Test.Tasty.Golden.Advanced

type QBEResult = (ExitCode, String)

runQBE :: FilePath -> IO QBEResult
runQBE filePath = do
  devNull <- openFile "/dev/null" WriteMode

  (_, _, Just herr, p) <-
    createProcess
      (proc "qbe" [filePath])
        { std_out = UseHandle devNull,
          std_err = CreatePipe
        }

  ret <- waitForProcess p <* hClose devNull
  out <- hGetContents herr
  return (ret, out)

runQuebex :: FilePath -> IO QBEResult
runQuebex filePath = do
  content <- readFile filePath
  case parse filePath content of
    Right _ -> pure (ExitSuccess, "")
    Left err -> pure (ExitFailure 1, show err)

simpleCmp :: QBEResult -> QBEResult -> IO (Maybe String)
simpleCmp (exit, out) (exit', out') =
  return $
    if exit == exit'
      then Nothing
      else Just ("Parsing mismatch: " ++ err)
  where
    err :: String
    err = "qbe=(" ++ show exit ++ "): " ++ show out ++ " quebex=(" ++ show exit' ++ "):" ++ show out'

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
    [ runTest "data-definition-whitespace",
      runTest "empty-definitions",
      runTest "function-definition",
      runTest "call-instruction"
    ]
