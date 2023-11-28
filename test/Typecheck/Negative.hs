module Typecheck.Negative where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Data.Effect.TaggedLock

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: TypeCheckerError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- defaultEntryPointIO' LockModeExclusive tRoot file'
            result <- runIOEither' LockModeExclusive entryPoint upToInternalTyped
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "An error ocurred but it was not in the type checker."
              Right _ -> assertFailure "The type checker did not find an error."
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

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Constructor in pattern type error"
      $(mkRelDir "Internal")
      $(mkRelFile "PatternConstructor.juvix")
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Check pattern with hole type"
      $(mkRelDir "265")
      $(mkRelFile "M.juvix")
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Type vs inferred type mismatch"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongType.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Function application with non-function type"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedFunctionType.juvix")
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unsolved hole"
      $(mkRelDir "Internal")
      $(mkRelFile "UnsolvedMeta.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple type errors are captured"
      $(mkRelDir "Internal")
      $(mkRelFile "MultiWrongType.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong return type name for a constructor of a simple data type"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnType.juvix")
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Too few arguments for the return type of a constructor"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnTypeTooFewArguments.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous hole"
      $(mkRelDir "Internal")
      $(mkRelFile "IdenFunctionArgsNoExplicit.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Cycle in hole"
      $(mkRelDir "issue1700")
      $(mkRelFile "SelfApplication.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    NegTest
      "Negative integer literal cannot be used as a Nat"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralInteger.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    NegTest
      "Integer literal cannot be used as a String"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralIntegerString.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unsupported type function"
      $(mkRelDir "Internal")
      $(mkRelFile "UnsupportedTypeFunction.juvix")
      $ \case
        ErrUnsupportedTypeFunction {} -> Nothing
        _ -> wrongError,
    NegTest
      "Instance target not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "TargetNotATrait.juvix")
      $ \case
        ErrTargetNotATrait {} -> Nothing
        _ -> wrongError,
    NegTest
      "Not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "NotATrait.juvix")
      $ \case
        ErrNotATrait {} -> Nothing
        _ -> wrongError,
    NegTest
      "No instance"
      $(mkRelDir "Internal")
      $(mkRelFile "NoInstance.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous instances"
      $(mkRelDir "Internal")
      $(mkRelFile "AmbiguousInstances.juvix")
      $ \case
        ErrAmbiguousInstances {} -> Nothing
        _ -> wrongError,
    NegTest
      "Subsumed instance"
      $(mkRelDir "Internal")
      $(mkRelFile "SubsumedInstance.juvix")
      $ \case
        ErrSubsumedInstance {} -> Nothing
        _ -> wrongError,
    NegTest
      "Explicit instance argument"
      $(mkRelDir "Internal")
      $(mkRelFile "ExplicitInstanceArgument.juvix")
      $ \case
        ErrExplicitInstanceArgument {} -> Nothing
        _ -> wrongError,
    NegTest
      "Instance termination"
      $(mkRelDir "Internal")
      $(mkRelFile "InstanceTermination.juvix")
      $ \case
        ErrTraitNotTerminating {} -> Nothing
        _ -> wrongError,
    NegTest
      "Default value wrong type"
      $(mkRelDir "Internal")
      $(mkRelFile "DefaultTypeError.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Coercion target not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "CoercionTargetNotATrait.juvix")
      $ \case
        ErrTargetNotATrait {} -> Nothing
        _ -> wrongError,
    NegTest
      "Invalid coercion type"
      $(mkRelDir "Internal")
      $(mkRelFile "InvalidCoercionType.juvix")
      $ \case
        ErrInvalidCoercionType {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong coercion argument"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongCoercionArgument.juvix")
      $ \case
        ErrWrongCoercionArgument {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous coercions"
      $(mkRelDir "Internal")
      $(mkRelFile "AmbiguousCoercions.juvix")
      $ \case
        ErrAmbiguousInstances {} -> Nothing
        _ -> wrongError,
    NegTest
      "Coercion cycles"
      $(mkRelDir "Internal")
      $(mkRelFile "LoopingCoercion.juvix")
      $ \case
        ErrCoercionCycles {} -> Nothing
        _ -> wrongError
  ]

negPositivityTests :: [NegTest]
negPositivityTests =
  [ NegTest "E1" $(mkRelDir "Internal/Positivity") $(mkRelFile "E1.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E2" $(mkRelDir "Internal/Positivity") $(mkRelFile "E2.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E3" $(mkRelDir "Internal/Positivity") $(mkRelFile "E3.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E4" $(mkRelDir "Internal/Positivity") $(mkRelFile "E4.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E5" $(mkRelDir "Internal/Positivity") $(mkRelFile "E5.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E6" $(mkRelDir "Internal/Positivity") $(mkRelFile "E6.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E7" $(mkRelDir "Internal/Positivity") $(mkRelFile "E7.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E8" $(mkRelDir "Internal/Positivity") $(mkRelFile "E8.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError,
    NegTest "E9" $(mkRelDir "Internal/Positivity") $(mkRelFile "E9.juvix") $
      \case
        ErrNoPositivity {} -> Nothing
        _ -> wrongError
  ]
