{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Scope.Negative (allTests) where

import Base
import MiniJuvix.Syntax.Concrete.Scoped.Error
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M

type FailMsg = String

data NegTest = NegTest {
  name :: String,
  relDir :: FilePath,
  file :: FilePath,
  checkErr :: ScopeError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} = TestDescr {
  testName = name,
  testRoot = root </> relDir,
  testAssertion = Single $ do
      p <- parseModuleIO file >>= M.scopeCheck1IO "."
      case p of
        Left err -> whenJust (checkErr err) assertFailure
        Right _ -> assertFailure "The scope checker did not find an error."
  }

allTests :: TestTree
allTests = testGroup "Scope negative tests"
  (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests = [
  NegTest "Not in scope" "."
   "NotInScope.mjuvix" $ \case
      ErrSymNotInScope{} -> Nothing
      _ -> wrongError
  ,  NegTest "Multiple declarations" "."
   "MultipleDeclarations.mjuvix" $ \case
      ErrMultipleDeclarations{} -> Nothing
      _ -> wrongError
  ,  NegTest "Import cycle" "ImportCycle"
   "A.mjuvix" $ \case
      ErrImportCycle {} -> Nothing
      _ -> wrongError
  ,  NegTest "Binding group conflict (function clause)"
    "BindGroupConflict"
   "Clause.mjuvix" $ \case
      ErrBindGroup {} -> Nothing
      _ -> wrongError
  ,  NegTest "Binding group conflict (lambda clause)"
    "BindGroupConflict"
   "Lambda.mjuvix" $ \case
      ErrBindGroup {} -> Nothing
      _ -> wrongError

  ,  NegTest "Infix error (expression)"
    "."
   "InfixError.mjuvix" $ \case
      ErrInfixParser {} -> Nothing
      _ -> wrongError
  ,  NegTest "Infix error (pattern)"
    "."
   "InfixErrorP.mjuvix" $ \case
      ErrInfixPattern {} -> Nothing
      _ -> wrongError
  ,  NegTest "Duplicate fixity declaration"
    "."
   "DuplicateFixity.mjuvix" $ \case
      ErrDuplicateFixity {} -> Nothing
      _ -> wrongError
  ,  NegTest "Multiple export conflict"
    "."
   "MultipleExportConflict.mjuvix" $ \case
      ErrMultipleExport {} -> Nothing
      _ -> wrongError

  ,  NegTest "Module not in scope"
    "."
   "ModuleNotInScope.mjuvix" $ \case
      ErrModuleNotInScope {} -> Nothing
      _ -> wrongError

  ,  NegTest "Unused operator syntax definition"
    "."
   "UnusedOperatorDef.mjuvix" $ \case
      ErrUnusedOperatorDef {} -> Nothing
      _ -> wrongError
  ,  NegTest "Ambiguous symbol"
    "."
   "AmbiguousSymbol.mjuvix" $ \case
      ErrAmbiguousSym {} -> Nothing
      _ -> wrongError
  ,  NegTest "Lacks function clause"
    "."
   "LacksFunctionClause.mjuvix" $ \case
      ErrLacksFunctionClause {} -> Nothing
      _ -> wrongError
  ,  NegTest "Incorrect top module path"
    "."
   "WrongModuleName.mjuvix" $ \case
      ErrWrongTopModuleName {} -> Nothing
      _ -> wrongError
  ]
