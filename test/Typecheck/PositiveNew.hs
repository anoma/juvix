module Typecheck.PositiveNew where

import Base
import Data.HashSet qualified as HashSet
import Juvix.Data.Effect.TaggedLock
import Typecheck.Positive qualified as Old

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> Old.PosTest
posTest _name rdir rfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
   in Old.PosTest {..}

testDescr :: Old.PosTest -> TestDescr
testDescr Old.PosTest {..} =
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
    "New typechecker positive tests"
    [ testGroup
        "New typechecker General typechecking tests"
        (map (mkTest . testDescr) (filter (not . isIgnored) (extraTests <> Old.tests)))
    ]

isIgnored :: Old.PosTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

extraTests :: [Old.PosTest]
extraTests = []

-- | Default values are not supported by the new type checker at the moment
ignored :: HashSet String
ignored =
  HashSet.fromList
    [ -- This test does not pass with the new hole insertion algorithm
      "Test046: Polymorphic type arguments"
    ]
