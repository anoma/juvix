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
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified MiniJuvix.Syntax.Concrete.Parser as Parser
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as Scoper
import qualified MiniJuvix.Translation.ScopedToAbstract as A

parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseModuleIO = fromRightIO id . Parser.runModuleParserIO

parseTextModuleIO :: Text -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseTextModuleIO = fromRightIO id . return . Parser.runModuleParser "literal string"

scopeModuleIO :: M.Module 'M.Parsed 'M.ModuleTop -> IO (M.Module 'M.Scoped 'M.ModuleTop)
scopeModuleIO = fmap (head . Scoper._resultModules) . fromRightIO' printErrorAnsi . Scoper.scopeCheck1IO "."

translateModuleIO :: M.Module 'M.Scoped 'M.ModuleTop -> IO A.TopModule
translateModuleIO = fmap snd . fromRightIO id . return . A.translateModule

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
