module TypeCheck.Negative (allTests) where

import Base
import Juvix.Pipeline
import Juvix.Syntax.MicroJuvix.Error

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
            let entryPoint = defaultEntryPoint _file
            result <- runIOEither (upToMicroJuvixTyped entryPoint)
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "The type checker did not find an error."
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
      "PatternConstructor.juvix"
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Check pattern with hole type"
      "265"
      "M.juvix"
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Type vs inferred type mismatch"
      "MicroJuvix"
      "WrongType.juvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      "MicroJuvix"
      "ExpectedFunctionType.juvix"
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unsolved hole"
      "MicroJuvix"
      "UnsolvedMeta.juvix"
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      "MicroJuvix"
      "MultiWrongType.juvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unexpected braces in pattern"
      "issue1337"
      "Braces.juvix"
      $ \case
        ErrArity (ErrWrongPatternIsImplicit {}) -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong return type name for a constructor of a simple data type"
      "MicroJuvix"
      "WrongReturnType.juvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too few arguments for the return type of a constructor"
      "MicroJuvix"
      "WrongReturnTypeTooFewArguments.juvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too many arguments for the return type of a constructor"
      "MicroJuvix"
      "WrongReturnTypeTooManyArguments.juvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest "E1" "MicroJuvix/NoStrictlyPositiveDataTypes" "E1.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E2" "MicroJuvix/NoStrictlyPositiveDataTypes" "E2.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E3" "MicroJuvix/NoStrictlyPositiveDataTypes" "E3.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E4" "MicroJuvix/NoStrictlyPositiveDataTypes" "E4.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E5" "MicroJuvix/NoStrictlyPositiveDataTypes" "E5.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E6" "MicroJuvix/NoStrictlyPositiveDataTypes" "E6.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E7" "MicroJuvix/NoStrictlyPositiveDataTypes" "E7.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E8" "MicroJuvix/NoStrictlyPositiveDataTypes" "E8.juvix" $
      \case
        ErrNoStrictPositivity {} -> Nothing
        _ -> wrongError
  ]
