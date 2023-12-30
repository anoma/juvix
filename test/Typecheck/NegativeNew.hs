module Typecheck.NegativeNew where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Typecheck.Negative qualified as Old

type FailMsg = String

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

negTest :: String -> Path Rel Dir -> Path Rel File -> (TypeCheckerError -> Maybe FailMsg) -> Old.NegTest
negTest _name rdir rfile _checkErr =
  let _dir = root <//> rdir
   in Old.NegTest
        { _file = _dir <//> rfile,
          _name,
          _dir,
          _checkErr
        }

testDescr :: Old.NegTest -> TestDescr
testDescr Old.NegTest {..} =
  let tRoot = _dir
      file' = _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- testDefaultEntryPointIO tRoot file'
            result <- testRunIOEither entryPoint upToCore
            case mapLeft fromJuvixError result of
              Left (Just tyError) -> whenJust (_checkErr tyError) assertFailure
              Left Nothing -> assertFailure "An error ocurred but it was not in the type checker."
              Right _ -> assertFailure "The type checker did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "New typechecker negative tests"
    [ testGroup
        "New typechecker General negative typechecking tests"
        (map (mkTest . testDescr) (filter (not . isIgnored) Old.tests)),
      testGroup
        "Non-strictly positive data types"
        (map (mkTest . testDescr) Old.negPositivityTests),
      testGroup
        "Arity tests"
        (map (mkTest . testDescr) arityTests)
    ]

isIgnored :: Old.NegTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

ignored :: HashSet String
ignored =
  HashSet.fromList
    []

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

negArityTest :: String -> Path Rel Dir -> Path Rel File -> (ArityCheckerError -> Maybe FailMsg) -> Old.NegTest
negArityTest _name rdir rfile ariErr =
  let _dir = root <//> rdir
   in Old.NegTest
        { _file = _dir <//> rfile,
          _checkErr = \case
            ErrArityCheckerError e -> ariErr e
            e -> error (show e),
          _name,
          _dir
        }

arityTests :: [Old.NegTest]
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
