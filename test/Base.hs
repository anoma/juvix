module Base
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    module MiniJuvix.Prelude,
    module Base,
  )
where

import MiniJuvix.Prelude
import Test.Tasty
import Test.Tasty.HUnit

data AssertionDescr
  = Single Assertion
  | Steps ((String -> IO ()) -> Assertion)

data TestDescr = TestDescr
  { _testName :: String,
    _testRoot :: FilePath,
    -- | relative to root
    _testAssertion :: AssertionDescr
  }

makeLenses ''TestDescr

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} = case _testAssertion of
  Single assertion -> testCase _testName $ withCurrentDirectory _testRoot assertion
  Steps steps -> testCaseSteps _testName (withCurrentDirectory _testRoot . steps)
