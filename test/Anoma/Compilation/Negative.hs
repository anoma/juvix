module Anoma.Compilation.Negative where

import Base hiding (compileMain)
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Core.Error
import Juvix.Prelude qualified as Prelude

root :: Prelude.Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Compilation/negative")

type CheckError = JuvixError -> IO ()

mkAnomaNegativeTest :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> CheckError -> TestTree
mkAnomaNegativeTest testName' relRoot mainFile testCheck =
  testCase (unpack testName') mkTestIO
  where
    mkTestIO :: IO ()
    mkTestIO = do
      merr <- withRootCopy compileMain
      case merr of
        Nothing -> assertFailure "expected compilation to fail"
        Just err -> testCheck err

    withRootCopy :: (Prelude.Path Abs Dir -> IO a) -> IO a
    withRootCopy action = withSystemTempDir "test" $ \tmpRootDir -> do
      copyDirRecur root tmpRootDir
      action tmpRootDir

    compileMain :: Prelude.Path Abs Dir -> IO (Maybe JuvixError)
    compileMain rootCopyDir = do
      let testRootDir = rootCopyDir <//> relRoot
      entryPoint <-
        set entryPointPipeline (Just PipelineExec)
          . set entryPointTarget (Just TargetAnoma)
          <$> testDefaultEntryPointIO testRootDir (testRootDir <//> mainFile)
      either Just (const Nothing) <$> testRunIOEither entryPoint upToAnoma

checkCoreError :: CheckError
checkCoreError e =
  unless
    (isJust (fromJuvixError @CoreError e))
    (assertFailure ("Expected core error got: " <> unpack (renderTextDefault e)))

allTests :: TestTree
allTests =
  testGroup
    "Anoma negative tests"
    [mkAnomaNegativeTest "Use of Strings" $(mkRelDir ".") $(mkRelFile "String.juvix") checkCoreError]
