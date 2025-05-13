module Termination.Negative (module Termination.Negative) where

import Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    -- When False, the test will not be typechecked with the --no-termination flag
    _terminationCanBeSkipped :: Bool,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: TerminationError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- set entryPointNoStdlib True <$> testDefaultEntryPointIO tRoot file'
            result <- testRunIOEither entryPoint upToInternalTyped
            case mapLeft fromJuvixError result of
              Left (Just lexError) -> whenJust (_checkErr lexError) assertFailure
              Left Nothing -> assertFailure "The termination checker did not find an error."
              Right _ -> assertFailure "An error occurred but it was not by the termination checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "Termination negative tests"
    (map (mkTest . testDescr) tests)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative/Termination")

tests :: [NegTest]
tests =
  [ NegTest
      "Mutual recursive functions non terminating"
      True
      $(mkRelDir ".")
      $(mkRelFile "Mutual.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Another mutual block non terminating"
      True
      $(mkRelDir ".")
      $(mkRelFile "Ord.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, f, marked terminating in a mutual block"
      True
      $(mkRelDir ".")
      $(mkRelFile "TerminatingF.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, g, marked terminating in a mutual block"
      True
      $(mkRelDir ".")
      $(mkRelFile "TerminatingG.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Tree"
      True
      $(mkRelDir ".")
      $(mkRelFile "Data/Tree.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Quicksort is not terminating"
      True
      $(mkRelDir ".")
      $(mkRelFile "Data/QuickSort.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Loop in axiom type"
      False
      $(mkRelDir ".")
      $(mkRelFile "Axiom.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Loop with default arguments"
      True
      $(mkRelDir ".")
      $(mkRelFile "DefaultArguments.juvix")
      $ \case
        ErrNoLexOrder {} -> Nothing
  ]
