module Base (
 module Test.Tasty,
 module Test.Tasty.HUnit,
 module MiniJuvix.Prelude,
 module Base
 ) where

import Test.Tasty
import Test.Tasty.HUnit
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Language as M
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M

parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseModuleIO = fromRightIO id . M.runModuleParserIO

parseTextModuleIO :: Text -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseTextModuleIO = fromRightIO id . return . M.runModuleParser "literal string"

scopeModuleIO :: M.Module 'M.Parsed 'M.ModuleTop -> IO (M.Module 'M.Scoped 'M.ModuleTop)
scopeModuleIO = fromRightIO' printErrorAnsi . M.scopeCheck1IO "."

data AssertionDescr =
  Single Assertion
  | Steps ((String -> IO ()) -> Assertion)

data TestDescr = TestDescr {
  testName :: String,
  testRoot :: FilePath,
  -- | relative to root
  testAssertion :: AssertionDescr
  }

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} = case testAssertion of
  Single assertion -> testCase testName $ withCurrentDirectory testRoot assertion
  Steps steps -> testCaseSteps testName (withCurrentDirectory testRoot . steps)
