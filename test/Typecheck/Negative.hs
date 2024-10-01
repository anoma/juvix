module Typecheck.Negative where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _checkErr :: TypeCheckerError -> Maybe FailMsg
  }

makeLenses ''NegTest

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = _dir
      file' = _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- testDefaultEntryPointIO tRoot file'
            result <- testRunIOEither entryPoint upToInternalTyped
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "An error occurred but it was not in the type checker."
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
        (map (mkTest . testDescr) negPositivityTests),
      testGroup
        "Arity tests"
        (map (mkTest . testDescr) arityTests)
    ]

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

negTest :: String -> Path Rel Dir -> Path Rel File -> (TypeCheckerError -> Maybe FailMsg) -> NegTest
negTest _name rdir rfile _checkErr =
  let _dir = root <//> rdir
   in NegTest
        { _file = _dir <//> rfile,
          _name,
          _dir,
          _checkErr
        }

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ negTest
      "Constructor in pattern type error"
      $(mkRelDir "Internal")
      $(mkRelFile "PatternConstructor.juvix")
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    negTest
      "Check pattern with hole type"
      $(mkRelDir "265")
      $(mkRelFile "M.juvix")
      $ \case
        ErrWrongConstructorType {} -> Nothing
        _ -> wrongError,
    negTest
      "Type vs inferred type mismatch"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongType.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    negTest
      "Function application with non-function type"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedFunctionType.juvix")
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    negTest
      "Unsolved hole"
      $(mkRelDir "Internal")
      $(mkRelFile "UnsolvedMeta.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    negTest
      "Multiple type errors are captured"
      $(mkRelDir "Internal")
      $(mkRelFile "MultiWrongType.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    negTest
      "Unexpected braces in pattern"
      $(mkRelDir "issue1337")
      $(mkRelFile "Braces.juvix")
      $ \case
        ErrArityCheckerError (ErrWrongPatternIsImplicit {}) -> Nothing
        _ -> wrongError,
    negTest
      "Unexpected double braces in pattern"
      $(mkRelDir "issue1337")
      $(mkRelFile "DoubleBraces.juvix")
      $ \case
        ErrArityCheckerError (ErrWrongPatternIsImplicit {}) -> Nothing
        _ -> wrongError,
    negTest
      "Wrong return type name for a constructor of a simple data type"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnType.juvix")
      $ \case
        ErrWrongReturnType {} -> Nothing
        _ -> wrongError,
    negTest
      "Too few arguments for the return type of a constructor"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnTypeTooFewArguments.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    negTest
      "Ambiguous hole"
      $(mkRelDir "Internal")
      $(mkRelFile "IdenFunctionArgsNoExplicit.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    negTest
      "Cycle in hole"
      $(mkRelDir "issue1700")
      $(mkRelFile "SelfApplication.juvix")
      $ \case
        ErrUnsolvedMeta {} -> Nothing
        _ -> wrongError,
    negTest
      "Negative integer literal cannot be used as a Nat"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralInteger.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    negTest
      "Integer literal cannot be used as a String"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralIntegerString.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    negTest
      "Unsupported type function"
      $(mkRelDir "Internal")
      $(mkRelFile "UnsupportedTypeFunction.juvix")
      $ \case
        ErrUnsupportedTypeFunction {} -> Nothing
        _ -> wrongError,
    negTest
      "Instance target not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "TargetNotATrait.juvix")
      $ \case
        ErrTargetNotATrait {} -> Nothing
        _ -> wrongError,
    negTest
      "Not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "NotATrait.juvix")
      $ \case
        ErrNotATrait {} -> Nothing
        _ -> wrongError,
    negTest
      "No instance"
      $(mkRelDir "Internal")
      $(mkRelFile "NoInstance.juvix")
      $ \case
        ErrNoInstance {} -> Nothing
        _ -> wrongError,
    negTest
      "Ambiguous instances"
      $(mkRelDir "Internal")
      $(mkRelFile "AmbiguousInstances.juvix")
      $ \case
        ErrAmbiguousInstances {} -> Nothing
        _ -> wrongError,
    negTest
      "Subsumed instance"
      $(mkRelDir "Internal")
      $(mkRelFile "SubsumedInstance.juvix")
      $ \case
        ErrSubsumedInstance {} -> Nothing
        _ -> wrongError,
    negTest
      "Explicit instance argument"
      $(mkRelDir "Internal")
      $(mkRelFile "ExplicitInstanceArgument.juvix")
      $ \case
        ErrExplicitInstanceArgument {} -> Nothing
        _ -> wrongError,
    negTest
      "Instance termination"
      $(mkRelDir "Internal")
      $(mkRelFile "InstanceTermination.juvix")
      $ \case
        ErrTraitNotTerminating {} -> Nothing
        _ -> wrongError,
    negTest
      "Default value wrong type"
      $(mkRelDir "Internal")
      $(mkRelFile "DefaultTypeError.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError,
    negTest
      "Coercion target not a trait"
      $(mkRelDir "Internal")
      $(mkRelFile "CoercionTargetNotATrait.juvix")
      $ \case
        ErrTargetNotATrait {} -> Nothing
        _ -> wrongError,
    negTest
      "Invalid coercion type"
      $(mkRelDir "Internal")
      $(mkRelFile "InvalidCoercionType.juvix")
      $ \case
        ErrInvalidCoercionType {} -> Nothing
        _ -> wrongError,
    negTest
      "Wrong coercion argument"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongCoercionArgument.juvix")
      $ \case
        ErrWrongCoercionArgument {} -> Nothing
        _ -> wrongError,
    negTest
      "Ambiguous coercions"
      $(mkRelDir "Internal")
      $(mkRelFile "AmbiguousCoercions.juvix")
      $ \case
        ErrAmbiguousInstances {} -> Nothing
        _ -> wrongError,
    negTest
      "Coercion cycles"
      $(mkRelDir "Internal")
      $(mkRelFile "LoopingCoercion.juvix")
      $ \case
        ErrCoercionCycles {} -> Nothing
        _ -> wrongError,
    negTest
      "Wrong type (issue 2771)"
      $(mkRelDir "issue2771")
      $(mkRelFile "Main.juvix")
      $ \case
        ErrWrongType {} -> Nothing
        _ -> wrongError
  ]

negPositivityTests :: [NegTest]
negPositivityTests =
  [ mk "E1" $(mkRelFile "E1.juvix"),
    mk "E2" $(mkRelFile "E2.juvix"),
    mk "E3" $(mkRelFile "E3.juvix"),
    mk "E4" $(mkRelFile "E4.juvix"),
    mk "E5" $(mkRelFile "E5.juvix"),
    mk "E6" $(mkRelFile "E6.juvix"),
    mk "E7" $(mkRelFile "E7.juvix"),
    mk "E8" $(mkRelFile "E8.juvix"),
    mk "E9" $(mkRelFile "E9.juvix"),
    mk "E10 uses type synonym" $(mkRelFile "E10.juvix"),
    mk "E11 uses type synonym" $(mkRelFile "E11.juvix"),
    mk "Box left hand side" $(mkRelFile "box.juvix")
  ]
  where
    mk :: String -> Path Rel File -> NegTest
    mk testname testfile = negTest testname $(mkRelDir "Internal/Positivity") testfile $
      \case
        ErrNonStrictlyPositive NonStrictlyPositive {} -> Nothing
        _ -> wrongError

arityTests :: [NegTest]
arityTests =
  [ negTest
      "Too many arguments in expression"
      $(mkRelDir "Internal")
      $(mkRelFile "TooManyArguments.juvix")
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    negTest
      "Pattern match a function type"
      $(mkRelDir "Internal")
      $(mkRelFile "FunctionPattern.juvix")
      $ \case
        ErrInvalidPatternMatching {} -> Nothing
        _ -> wrongError,
    negTest
      "Function type (* → *) application"
      $(mkRelDir "Internal")
      $(mkRelFile "FunctionApplied.juvix")
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    negArityTest
      "Expected explicit pattern"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedExplicitPattern.juvix")
      $ \case
        ErrWrongPatternIsImplicit {} -> Nothing
        _ -> wrongError,
    negArityTest
      "Expected explicit argument"
      $(mkRelDir "Internal")
      $(mkRelFile "ExpectedExplicitArgument.juvix")
      $ \case
        ErrExpectedExplicitArgument {} -> Nothing
        _ -> wrongError,
    negArityTest
      "Function clause with two many patterns in the lhs"
      $(mkRelDir "Internal")
      $(mkRelFile "LhsTooManyPatterns.juvix")
      $ \case
        ErrLhsTooManyPatterns {} -> Nothing
        _ -> wrongError,
    negTest
      "Too many arguments for the return type of a constructor"
      $(mkRelDir "Internal")
      $(mkRelFile "WrongReturnTypeTooManyArguments.juvix")
      $ \case
        ErrExpectedFunctionType {} -> Nothing
        _ -> wrongError,
    negArityTest
      "Lazy builtin not fully applied"
      $(mkRelDir "Internal")
      $(mkRelFile "LazyBuiltin.juvix")
      $ \case
        ErrBuiltinNotFullyApplied {} -> Nothing
        _ -> wrongError,
    negArityTest
      "issue 2293: Non-terminating function with arity error"
      $(mkRelDir "Internal")
      $(mkRelFile "issue2293.juvix")
      $ \case
        ErrWrongConstructorAppLength {} -> Nothing
        _ -> wrongError,
    negTest
      "Detect default argument cycle in the arity checker"
      $(mkRelDir "Internal")
      $(mkRelFile "DefaultArgCycleArity.juvix")
      $ \case
        ErrDefaultArgLoop {} -> Nothing
        _ -> wrongError,
    negTest "Evil: issue 2540" $(mkRelDir "Internal/Positivity") $(mkRelFile "Evil.juvix") $
      \case
        ErrNonStrictlyPositive ErrTypeAsArgumentOfBoundVar {} -> Nothing
        _ -> wrongError,
    negTest "Evil: issue 2540 using Axiom" $(mkRelDir "Internal/Positivity") $(mkRelFile "EvilWithAxiom.juvix") $
      \case
        ErrNonStrictlyPositive (ErrTypeAsArgumentOfBoundVar {}) -> Nothing
        _ -> wrongError,
    negTest "FreeT: issue 2540" $(mkRelDir "Internal/Positivity") $(mkRelFile "FreeT.juvix") $
      \case
        ErrNonStrictlyPositive (ErrTypeAsArgumentOfBoundVar {}) -> Nothing
        _ -> wrongError
  ]

negArityTest :: String -> Path Rel Dir -> Path Rel File -> (ArityCheckerError -> Maybe FailMsg) -> NegTest
negArityTest _name rdir rfile ariErr =
  let _dir = root <//> rdir
   in NegTest
        { _file = _dir <//> rfile,
          _checkErr = \case
            ErrArityCheckerError e -> ariErr e
            e -> error (show e),
          _name,
          _dir
        }
