module Base
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    module MiniJuvix.Prelude,
    module Base,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language qualified as A
import MiniJuvix.Syntax.Concrete.Language qualified as M
import MiniJuvix.Syntax.Concrete.Parser qualified as M
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as M
import MiniJuvix.Translation.ScopedToAbstract qualified as A
import Test.Tasty
import Test.Tasty.HUnit

parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseModuleIO = fromRightIO id . M.runModuleParserIO

parseTextModuleIO :: Text -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseTextModuleIO = fromRightIO id . return . M.runModuleParser "literal string"

scopeModuleIO :: M.Module 'M.Parsed 'M.ModuleTop -> IO (M.Module 'M.Scoped 'M.ModuleTop)
scopeModuleIO = fmap snd . fromRightIO' printErrorAnsi . M.scopeCheck1IO "."

translateModuleIO :: M.Module 'M.Scoped 'M.ModuleTop -> IO A.TopModule
translateModuleIO = fmap snd . fromRightIO id . return . A.translateModule

data AssertionDescr
  = Single Assertion
  | Steps ((String -> IO ()) -> Assertion)

data TestDescr = TestDescr
  { testName :: String,
    testRoot :: FilePath,
    -- | relative to root
    testAssertion :: AssertionDescr
  }

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} = case testAssertion of
  Single assertion -> testCase testName $ withCurrentDirectory testRoot assertion
  Steps steps -> testCaseSteps testName (withCurrentDirectory testRoot . steps)
