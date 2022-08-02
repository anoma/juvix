module Arity.Negative (allTests) where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Compiler.Pipeline

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: ArityCheckerError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint _file
            result <- runIOEither (upToInternalArity entryPoint)
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "The arity checker did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not in the arity checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "Arity checker negative tests"
    (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Too many arguments in expression"
      "Internal"
      "TooManyArguments.juvix"
      $ \case
        ErrTooManyArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Pattern match a function type"
      "Internal"
      "FunctionPattern.juvix"
      $ \case
        ErrPatternFunction {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function type (* → *) application"
      "Internal"
      "FunctionApplied.juvix"
      $ \case
        ErrFunctionApplied {} -> Nothing
        _ -> wrongError,
    NegTest
      "Expected explicit pattern"
      "Internal"
      "ExpectedExplicitPattern.juvix"
      $ \case
        ErrWrongPatternIsImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "Expected explicit argument"
      "Internal"
      "ExpectedExplicitArgument.juvix"
      $ \case
        ErrExpectedExplicitArgument {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function clause with two many patterns in the lhs"
      "Internal"
      "LhsTooManyPatterns.juvix"
      $ \case
        ErrLhsTooManyPatterns {} -> Nothing
        _ -> wrongError
  ]
