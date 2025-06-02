module Resolver.Negative where

import Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Error

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _checkErr :: PathResolverError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = _dir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- testDefaultEntryPointIO tRoot _file
            res <- testRunIOEither entryPoint upToParsedSource
            case mapLeft fromJuvixError res of
              Left (Just parErr) -> whenJust (_checkErr parErr) assertFailure
              Left Nothing -> assertFailure "An error occurred but it was not in the path resolver."
              Right _ -> assertFailure "The path resolver did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Path resolver negative tests"
    ( map (mkTest . testDescr) resolverErrorTests
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

negTest :: String -> Path Rel Dir -> Path Rel File -> (PathResolverError -> Maybe FailMsg) -> NegTest
negTest _name d f _checkErr = negTestAbsDir _name (root <//> d) f _checkErr

negTestAbsDir :: String -> Path Abs Dir -> Path Rel File -> (PathResolverError -> Maybe FailMsg) -> NegTest
negTestAbsDir _name _dir f _checkErr =
  NegTest
    { _file = _dir <//> f,
      _dir,
      _name,
      _checkErr
    }

resolverErrorTests :: [NegTest]
resolverErrorTests =
  [ negTest
      "Importing a module that conflicts with a module in the stdlib"
      $(mkRelDir "StdlibConflict")
      $(mkRelFile "Input.juvix")
      $ \case
        ErrDependencyConflict
          DependencyConflict {} -> Nothing
        _ -> wrongError,
    negTest
      "Same package name different version"
      $(mkRelDir "PackageNameConflict")
      $(mkRelFile "Main.juvix")
      $ \case
        ErrPackageNameConflict
          PackageNameConflict {} -> Nothing
        _ -> wrongError,
    negTest
      "Import a module that doesn't exist"
      $(mkRelDir "NoDependencies")
      $(mkRelFile "InvalidImport.juvix")
      $ \case
        ErrMissingModule
          MissingModule {} -> Nothing
        _ -> wrongError
  ]
