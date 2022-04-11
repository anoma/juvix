module Scope.Negative (allTests) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Syntax.Concrete.Scoped.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: ScopeError -> Maybe FailMsg
  }

root :: FilePath
root = "tests/negative"

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = EntryPoint "." (pure _file)
            res <- runIOEither (upToScoping entryPoint)
            let msg1 = "The scope checker did not find an error."
            let msg2 = "An error ocurred but it was not in the scoper."
            case mapLeft fromAJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure msg1
              Right _ -> assertFailure msg2
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope negative tests"
    (map (mkTest . testDescr) tests)

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Not in scope"
      "."
      "NotInScope.mjuvix"
      $ \case
        ErrSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple declarations"
      "."
      "MultipleDeclarations.mjuvix"
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Import cycle"
      "ImportCycle"
      "A.mjuvix"
      $ \case
        ErrImportCycle {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (function clause)"
      "BindGroupConflict"
      "Clause.mjuvix"
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (lambda clause)"
      "BindGroupConflict"
      "Lambda.mjuvix"
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (expression)"
      "."
      "InfixError.mjuvix"
      $ \case
        ErrInfixParser {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (pattern)"
      "."
      "InfixErrorP.mjuvix"
      $ \case
        ErrInfixPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Duplicate fixity declaration"
      "."
      "DuplicateFixity.mjuvix"
      $ \case
        ErrDuplicateFixity {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple export conflict"
      "."
      "MultipleExportConflict.mjuvix"
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Module not in scope"
      "."
      "ModuleNotInScope.mjuvix"
      $ \case
        ErrModuleNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unused operator syntax definition"
      "."
      "UnusedOperatorDef.mjuvix"
      $ \case
        ErrUnusedOperatorDef {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous symbol"
      "."
      "AmbiguousSymbol.mjuvix"
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause"
      "."
      "LacksFunctionClause.mjuvix"
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Incorrect top module path"
      "."
      "WrongModuleName.mjuvix"
      $ \case
        ErrWrongTopModuleName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous export"
      "."
      "AmbiguousExport.mjuvix"
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested modules"
      "."
      "AmbiguousModule.mjuvix"
      $ \case
        ErrAmbiguousModuleSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested constructors"
      "."
      "AmbiguousConstructor.mjuvix"
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError
  ]
