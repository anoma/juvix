module Termination.Negative (module Termination.Negative) where

import Base
import Juvix.Analysis.Termination
import Juvix.Pipeline

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: TerminationError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = (defaultEntryPoint _file) {_entryPointNoStdlib = True}
            result <- runIOEither (upToMicroJuvix entryPoint)
            case mapLeft fromJuvixError result of
              Left (Just lexError) -> whenJust (_checkErr lexError) assertFailure
              Left Nothing -> assertFailure "The termination checker did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not by the termination checker."
        }

allTests :: TestTree
allTests =
  testGroup
    "Termination negative tests"
    (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative/Termination"

tests :: [NegTest]
tests =
  [ NegTest
      "Mutual recursive functions non terminating"
      "."
      "Mutual.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Another mutual block non terminating"
      "."
      "Ord.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, f, marked terminating in a mutual block"
      "."
      "TerminatingF.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, g, marked terminating in a mutual block"
      "."
      "TerminatingG.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "f x := f x is not terminating"
      "."
      "ToEmpty.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Tree"
      "."
      "Data/Tree.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Quicksort is not terminating"
      "."
      "Data/QuickSort.juvix"
      $ \case
        ErrNoLexOrder {} -> Nothing
  ]
