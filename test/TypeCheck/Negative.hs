module TypeCheck.Negative (allTests) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Prelude.Error as Error
import MiniJuvix.Syntax.MicroJuvix.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: TypeCheckerError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = EntryPoint "." (pure _file)
            result <- runIOEither (upToMicroJuvixTyped entryPoint)
            case result of
              Left err -> case fromAJuvixError err of
                Just tyError -> whenJust (_checkErr tyError) assertFailure
                Nothing -> assertFailure ("The type checker did not find an error.\nThere is another error:\n" <> unpack (Error.renderText err))
              Right _ -> assertFailure "An error ocurred but it was not in the type checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "TypeCheck negative tests"
    (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Constructor in pattern type error"
      "MicroJuvix"
      "PatternConstructor.mjuvix"
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Constructor pattern length mismatch"
      "MicroJuvix"
      "PatternConstructorApp.mjuvix"
      $ \case
        ErrWrongConstructorAppArgs {} -> Nothing
        _ -> wrongError,
    NegTest
      "Type vs inferred type mismatch"
      "MicroJuvix"
      "WrongType.mjuvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      "MicroJuvix"
      "ExpectedFunctionType.mjuvix"
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function definition clause with two many match patterns"
      "MicroJuvix"
      "TooManyPatterns.mjuvix"
      $ \case
        ErrTooManyPatterns {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      "MicroJuvix"
      "MultiWrongType.mjuvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Constructor pattern with arity greater than the constructor"
      "MicroJuvix"
      "WrongConstructorArity.mjuvix"
      $ \case
        ErrWrongConstructorAppArgs {} -> Nothing
        _ -> wrongError
  ]
