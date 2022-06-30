module Termination.Negative (module Termination.Negative) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Termination

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
            case mapLeft fromMiniJuvixError result of
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
      "Mutual.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Another mutual block non terminating"
      "."
      "Ord.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, f, marked terminating in a mutual block"
      "."
      "TerminatingF.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Only one function, g, marked terminating in a mutual block"
      "."
      "TerminatingG.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "f x := f x is not terminating"
      "."
      "ToEmpty.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Tree"
      "."
      "Data/Tree.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing,
    NegTest
      "Quicksort is not terminating"
      "."
      "Data/QuickSort.mjuvix"
      $ \case
        ErrNoLexOrder {} -> Nothing
  ]
