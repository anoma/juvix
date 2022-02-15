module Scope.Negative (allTests) where

import Base
import MiniJuvix.Syntax.Concrete.Scoped.Error (ScopeError(..))
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M

type FailMsg = String

data NegTest = NegTest {
  name :: String,
  dir :: Maybe FilePath,
  file :: FilePath,
  checkErr :: ScopeError -> Maybe FailMsg
  }

instance IsTest NegTest where
  testDescr NegTest {..} = TestDescr {
    testName = name,
    testRoot = maybe root (root </>) dir,
    testAssertion = do
        p <- parseModuleIO file >>= M.scopeCheck1 "."
        case p of
          Left err -> whenJust (checkErr err) assertFailure
          Right _ -> assertFailure "The scope checker did not find an error."
    }

root :: FilePath
root = "tests/negative"

wrongError = Just "Incorrect error"

tests :: [NegTest]
tests = [
  NegTest "Not in scope" Nothing
   "NotInScope.mjuvix" $ (\er ->
      case er of
        ErrSymNotInScope{} -> Nothing
        _ -> wrongError)
  ,  NegTest "Multiple declarations" Nothing
   "MultipleDeclarations.mjuvix" $ (\er ->
      case er of
        ErrMultipleDeclarations{} -> Nothing
        _ -> wrongError)

  ]

allTests :: [ATest]
allTests = map ATest tests
