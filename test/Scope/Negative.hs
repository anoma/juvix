{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Scope.Negative (allTests) where

import Base
import MiniJuvix.Syntax.Concrete.Scoped.Error (ScopeError(..))
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M

type FailMsg = String

data NegTest = NegTest {
  name :: String,
  relDir :: FilePath,
  file :: FilePath,
  checkErr :: ScopeError -> Maybe FailMsg
  }

instance IsTest NegTest where
  testDescr NegTest {..} = TestDescr {
    testName = name,
    testRoot = root </> relDir,
    testAssertion = do
        p <- parseModuleIO file >>= M.scopeCheck1 "."
        case p of
          Left err -> whenJust (checkErr err) assertFailure
          Right _ -> assertFailure "The scope checker did not find an error."
    }

allTests :: [ATest]
allTests = map ATest tests

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests = [
  NegTest "Not in scope" "."
   "NotInScope.mjuvix" $ \er ->
      case er of
        ErrSymNotInScope{} -> Nothing
        _ -> wrongError

  ,  NegTest "Multiple declarations" "."
   "MultipleDeclarations.mjuvix" $ \er ->
      case er of
        ErrMultipleDeclarations{} -> Nothing
        _ -> wrongError

  ,  NegTest "Import cycle" "ImportCycle"
   "A.mjuvix" $ \er ->
      case er of
        ErrImportCycle {} -> Nothing
        _ -> wrongError

  ,  NegTest "Binding group conflict (function clause)"
    "BindGroupConflict"
   "Clause.mjuvix" $ \er ->
      case er of
        ErrBindGroup {} -> Nothing
        _ -> wrongError

  ,  NegTest "Binding group conflict (lambda clause)"
    "BindGroupConflict"
   "Lambda.mjuvix" $ \er ->
      case er of
        ErrBindGroup {} -> Nothing
        _ -> wrongError

  ,  NegTest "Infix error (expression)"
    "."
   "InfixError.mjuvix" $ \er ->
      case er of
        ErrInfixParser {} -> Nothing
        _ -> wrongError

  ,  NegTest "Infix error (pattern)"
    "."
   "InfixErrorP.mjuvix" $ \er ->
      case er of
        ErrInfixPattern {} -> Nothing
        _ -> wrongError

  ,  NegTest "Duplicate fixity declaration"
    "."
   "DuplicateFixity.mjuvix" $ \er ->
      case er of
        ErrDuplicateFixity {} -> Nothing
        _ -> wrongError

  ,  NegTest "Multiple export conflict"
    "."
   "MultipleExportConflict.mjuvix" $ \er ->
      case er of
        ErrMultipleExport {} -> Nothing
        _ -> wrongError

  ,  NegTest "Module not in scope"
    "."
   "ModuleNotInScope.mjuvix" $ \er ->
      case er of
        ErrModuleNotInScope {} -> Nothing
        _ -> wrongError
  ]
