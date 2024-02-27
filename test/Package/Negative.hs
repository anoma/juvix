module Package.Negative where

import Base
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Data.Effect.TaggedLock

type FailMsg = String

data NegTest a = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _checkErr :: a -> Maybe FailMsg
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative/Package")

testDescr :: (Typeable a) => NegTest a -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            res <-
              withTempDir'
                ( runM
                    . runResource
                    . runError
                    . runFilesIO
                    . mapError (JuvixError @PackageLoaderError)
                    . runTaggedLock LockModeExclusive
                    . runEvalFileEffIO
                    . readPackage tRoot
                    . CustomBuildDir
                    . Abs
                )
            case mapLeft fromJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure "An error occurred but it was not when reading the package."
              Right {} -> assertFailure "There was no error when reading the package"
        }

allTests :: TestTree
allTests =
  testGroup
    "Package loading negative tests"
    ( map (mkTest . testDescr) packageErrorTests
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

packageErrorTests :: [NegTest PackageLoaderError]
packageErrorTests =
  [ NegTest
      "package YAML parse error"
      $(mkRelDir "YamlParseError")
      $ \case
        PackageLoaderError _ ErrPackageYamlParseError {} -> Nothing
        _ -> wrongError,
    NegTest
      "lockfile YAML parse error"
      $(mkRelDir "InvalidLockfile")
      $ \case
        PackageLoaderError _ ErrLockfileYamlParseError {} -> Nothing
        _ -> wrongError,
    NegTest
      "package YAML invalid version"
      $(mkRelDir "YamlInvalidVersion")
      $ \case
        PackageLoaderError _ ErrVersionParseError {} -> Nothing
        _ -> wrongError,
    NegTest
      "package YAML duplicate dependencies"
      $(mkRelDir "YamlDuplicateDependencies")
      $ \case
        PackageLoaderError _ ErrDuplicateDependencyError {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix doesn't compile"
      $(mkRelDir "PackageJuvixDoesntCompile")
      $ \case
        PackageLoaderError _ ErrPackageJuvixError {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix no package symbol"
      $(mkRelDir "PackageJuvixNoPackageSymbol")
      $ \case
        PackageLoaderError _ ErrPackageSymbolNotFound {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix package symbol has wrong type1"
      $(mkRelDir "PackageJuvixPackageSymbolWrongType1")
      $ \case
        PackageLoaderError _ ErrPackageTypeError {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix package symbol has wrong type2"
      $(mkRelDir "PackageJuvixPackageSymbolWrongType2")
      $ \case
        PackageLoaderError _ ErrPackageTypeError {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix duplicate dependencies"
      $(mkRelDir "PackageJuvixDuplicateDependencies")
      $ \case
        PackageLoaderError _ ErrDuplicateDependencyError {} -> Nothing
        _ -> wrongError,
    NegTest
      "Package.juvix lockfile YAML parse error"
      $(mkRelDir "InvalidLockfile")
      $ \case
        PackageLoaderError _ ErrLockfileYamlParseError {} -> Nothing
        _ -> wrongError
  ]
