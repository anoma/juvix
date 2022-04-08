module TypeCheck.Negative (allTests) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Syntax.MicroJuvix.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: TypeCheckerErrors -> Maybe FailMsg
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
            let msg1 = "The type checker did not find an error."
            let msg2 = "An error ocurred but it was not in the type checker."
            case mapLeft fromAJuvixError result of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure msg1
              Right _ -> assertFailure msg2
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
        (TypeCheckerErrors (ErrWrongConstructorType {} :| [])) -> Nothing
        _ -> wrongError,
    NegTest
      "Constructor pattern length mismatch"
      "MicroJuvix"
      "PatternConstructorApp.mjuvix"
      $ \case
        (TypeCheckerErrors (ErrWrongConstructorAppArgs {} :| [])) -> Nothing
        _ -> wrongError,
    NegTest
      "Type vs inferred type mismatch"
      "MicroJuvix"
      "WrongType.mjuvix"
      $ \case
        (TypeCheckerErrors (ErrWrongType {} :| [])) -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      "MicroJuvix"
      "ExpectedFunctionType.mjuvix"
      $ \case
        (TypeCheckerErrors (ErrExpectedFunctionType {} :| [])) -> Nothing
        _ -> wrongError,
    NegTest
      "Function definition clause with two many match patterns"
      "MicroJuvix"
      "TooManyPatterns.mjuvix"
      $ \case
        (TypeCheckerErrors (ErrTooManyPatterns {} :| [])) -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      "MicroJuvix"
      "MultiWrongType.mjuvix"
      $ \case
        (TypeCheckerErrors (ErrWrongType {} :| [ErrWrongType {}])) -> Nothing
        _ -> wrongError
  ]
