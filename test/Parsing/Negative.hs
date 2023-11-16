module Parsing.Negative where

import Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error
import Juvix.Data.Effect.TaggedLock
import Juvix.Parser.Error

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _checkErr :: ParserError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = _dir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- defaultEntryPointIO' LockModeExclusive tRoot _file
            res <- runIOEither' LockModeExclusive entryPoint upToParsing
            case mapLeft fromJuvixError res of
              Left (Just parErr) -> whenJust (_checkErr parErr) assertFailure
              Left Nothing -> assertFailure "An error ocurred but it was not in the parser."
              Right _ -> assertFailure "The parser did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Parsing negative tests"
    ( map (mkTest . testDescr) (parserErrorTests <> filesErrorTests)
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

negTest :: String -> Path Rel Dir -> Path Rel File -> (ParserError -> Maybe FailMsg) -> NegTest
negTest _name d f _checkErr = negTestAbsDir _name (root <//> d) f _checkErr

negTestAbsDir :: String -> Path Abs Dir -> Path Rel File -> (ParserError -> Maybe FailMsg) -> NegTest
negTestAbsDir _name _dir f _checkErr =
  NegTest
    { _file = _dir <//> f,
      _dir,
      _name,
      _checkErr
    }

parserErrorTests :: [NegTest]
parserErrorTests =
  [ negTest
      "Tab character"
      $(mkRelDir ".")
      $(mkRelFile "Tab.juvix")
      $ \case
        ErrMegaparsec {} -> Nothing
        _ -> wrongError,
    negTest
      "Pragmas YAML error"
      $(mkRelDir ".")
      $(mkRelFile "PragmasYAML.juvix")
      $ \case
        ErrMegaparsec {} -> Nothing
        _ -> wrongError,
    negTest
      "Pragmas format error"
      $(mkRelDir ".")
      $(mkRelFile "PragmasFormat.juvix")
      $ \case
        ErrMegaparsec {} -> Nothing
        _ -> wrongError,
    negTest
      "Pragmas duplicate keys error"
      $(mkRelDir ".")
      $(mkRelFile "PragmasDuplicateKeys.juvix")
      $ \case
        ErrMegaparsec {} -> Nothing
        _ -> wrongError,
    negTest
      "Error on local instances"
      $(mkRelDir ".")
      $(mkRelFile "ErrorOnLocalInstances.juvix")
      $ \case
        ErrMegaparsec {} -> Nothing
        _ -> wrongError
  ]

filesErrorTests :: [NegTest]
filesErrorTests =
  [ negTest
      "Importing a module that conflicts with a module in the stdlib"
      $(mkRelDir "StdlibConflict")
      $(mkRelFile "Input.juvix")
      $ \case
        ErrTopModulePath
          TopModulePathError {_topModulePathError = ErrDependencyConflict {}} -> Nothing
        _ -> wrongError,
    negTest
      "Incorrect top module path"
      $(mkRelDir ".")
      $(mkRelFile "WrongModuleName.juvix")
      $ \case
        ErrWrongTopModuleName {} -> Nothing
        _ -> wrongError,
    negTestAbsDir
      "Incorrect top module path of an orphan file."
      (relToProject $(mkRelDir "tests/WithoutPackageFile"))
      $(mkRelFile "NoGood.juvix")
      $ \case
        ErrWrongTopModuleNameOrphan {} -> Nothing
        _ -> wrongError,
    negTest
      "Import a module that doesn't exist"
      $(mkRelDir "NoDependencies")
      $(mkRelFile "InvalidImport.juvix")
      $ \case
        ErrTopModulePath
          TopModulePathError {_topModulePathError = ErrMissingModule {}} -> Nothing
        _ -> wrongError,
    negTest
      "Dangling Judoc comment"
      $(mkRelDir ".")
      $(mkRelFile "DanglingJudoc.juvix")
      $ \case
        ErrDanglingJudoc
          DanglingJudoc {} -> Nothing
        _ -> wrongError
  ]
