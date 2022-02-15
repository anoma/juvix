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

fromRightIO' :: (e -> IO ()) -> IO (Either e r) -> IO r
fromRightIO' pp = do
  eitherM ifLeft return
  where
  ifLeft e = pp e >> exitFailure

fromRightIO :: (e -> Text) -> IO (Either e r) -> IO r
fromRightIO pp = fromRightIO' (putStrLn . pp)

scopeModuleIO :: M.Module 'M.Parsed 'M.ModuleTop -> IO (M.Module 'M.Scoped 'M.ModuleTop)
scopeModuleIO = fromRightIO' printErrorAnsi . M.scopeCheck1 "."

data TestDescr = TestDescr {
  testName :: String,
  testRoot :: FilePath,
  -- | relative to root
  testAssertion :: Assertion
  }

class IsTest t where
  testDescr :: t -> TestDescr

data ATest = forall t. IsTest t => ATest t

aTest :: ATest -> TestTree
aTest (ATest t) = mkTest (testDescr t)

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} =
  testCase testName $
  withCurrentDirectory testRoot testAssertion
