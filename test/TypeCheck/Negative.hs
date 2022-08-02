module TypeCheck.Negative where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Pipeline

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
            result <- runIOEither (upToInternalTyped entryPoint)
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "The type checker did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not in the type checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "Typecheck negative tests"
    [ testGroup
        "General typechecking errors"
        (map (mkTest . testDescr) tests),
      testGroup
        "Non-strictly positive data types"
        (map (mkTest . testDescr) negPositivityTests)
    ]

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Constructor in pattern type error"
      "Internal"
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
      "Internal"
      "WrongType.juvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      "Internal"
      "ExpectedFunctionType.juvix"
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unsolved hole"
      "Internal"
      "UnsolvedMeta.juvix"
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      "Internal"
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
      "Internal"
      "WrongReturnType.juvix"
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too few arguments for the return type of a constructor"
      "Internal"
      "WrongReturnTypeTooFewArguments.juvix"
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too many arguments for the return type of a constructor"
      "Internal"
      "WrongReturnTypeTooManyArguments.juvix"
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError
  ]

negPositivityTests :: [NegTest]
negPositivityTests =
  [ NegTest "E1" "Internal/Positivity" "E1.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E2" "Internal/Positivity" "E2.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E3" "Internal/Positivity" "E3.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E4" "Internal/Positivity" "E4.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E5" "Internal/Positivity" "E5.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E6" "Internal/Positivity" "E6.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E7" "Internal/Positivity" "E7.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E8" "Internal/Positivity" "E8.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E9" "Internal/Positivity" "E9.juvix" $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError
  ]
