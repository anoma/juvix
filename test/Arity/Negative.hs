module Arity.Negative (allTests) where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: ArityCheckerError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- defaultEntryPointCwdIO file'
            result <- runIOEitherTermination entryPoint upToInternalArity
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

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Too many arguments in expression"
      $(mkRelDir "Internal")
      $(mkRelFile "TooManyArguments.juvix")
      $ \case
        ErrTooManyArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Pattern match a function type"
      $(mkRelDir "Internal")
      $(mkRelFile "FunctionPattern.juvix")
      $ \case
        ErrPatternFunction {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function type (* → *) application"
      $(mkRelDir "Internal")
      $(mkRelFile "FunctionApplied.juvix")
      $ \case
        ErrFunctionApplied {} -> Nothing
        _ -> wrongError,
    NegTest
      "Expected explicit pattern"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedExplicitPattern.juvix")
      $ \case
        ErrWrongPatternIsImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "Expected explicit argument"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedExplicitArgument.juvix")
      $ \case
        ErrExpectedExplicitArgument {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function clause with two many patterns in the lhs"
      $(mkRelDir "Internal")
      $(mkRelFile "LhsTooManyPatterns.juvix")
      $ \case
        ErrLhsTooManyPatterns {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too many arguments for the return type of a constructor"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnTypeTooManyArguments.juvix")
      $ \case
        ErrTooManyArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lazy builtin not fully applied"
      $(mkRelDir "Internal")
      $(mkRelFile "LazyBuiltin.juvix")
      $ \case
        ErrBuiltinNotFullyApplied {} -> Nothing
        _ -> wrongError,
    NegTest
      "issue 2293: Non-terminating function with arity error"
      $(mkRelDir "Internal")
      $(mkRelFile "issue2293.juvix")
      $ \case
        ErrWrongConstructorAppLength {} -> Nothing
        _ -> wrongError
  ]
