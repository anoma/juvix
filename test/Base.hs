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

-- parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
-- parseModuleIO = fromRightIO id . Parser.runModuleParserIO

-- parseTextModuleIO :: Text -> IO (M.Module 'M.Parsed 'M.ModuleTop)
-- parseTextModuleIO = fromRightIO id . return . Parser.runModuleParser "literal string"

-- scopeModuleIO :: M.Module 'M.Parsed 'M.ModuleTop -> IO (M.Module 'M.Scoped 'M.ModuleTop)
-- scopeModuleIO = fmap (head . Scoper._resultModules) . fromRightIO' printErrorAnsi . Scoper.scopeCheck1IO "."

-- translateModuleIO :: M.Module 'M.Scoped 'M.ModuleTop -> IO A.TopModule
-- translateModuleIO = fmap snd . fromRightIO id . return . A.translateModule

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
