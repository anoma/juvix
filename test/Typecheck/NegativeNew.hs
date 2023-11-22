module Typecheck.NegativeNew where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.TaggedLock
import Typecheck.Negative qualified as Old

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

negTest :: String -> Path Rel Dir -> Path Rel File -> Old.NegTest
negTest _name rdir rfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
   in Old.NegTest {..}

testDescr :: Old.NegTest -> TestDescr
testDescr Old.NegTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Single $ do
        entryPoint <- set entryPointNewTypeCheckingAlgorithm True <$> defaultEntryPointIO' LockModeExclusive _dir _file
        (void . runIOExclusive entryPoint) upToInternalTyped
    }

allTests :: TestTree
allTests =
  testGroup
    "New typechecker negative tests"
    [ testGroup
        "New typechecker General negative typechecking tests"
        (map (mkTest . testDescr) (filter (not . isIgnored) Old.tests))
    ]

isIgnored :: Old.NegTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

ignored :: HashSet String
ignored =
  HashSet.fromList
    [
    ]
