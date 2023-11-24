module Typecheck.NegativeNew where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Data.Effect.TaggedLock
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
            entryPoint <- set entryPointNewTypeCheckingAlgorithm True <$> defaultEntryPointIO' LockModeExclusive tRoot file'
            result <- runIOEither' LockModeExclusive entryPoint upToInternalTyped
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
        (map (mkTest . testDescr) Old.negPositivityTests)
    ]

isIgnored :: Old.NegTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

ignored :: HashSet String
ignored =
  HashSet.fromList
    []
