module Scope.Negative (allTests) where

import Base
import MiniJuvix.Pipeline
import MiniJuvix.Syntax.Concrete.Scoped.Error

type FailMsg = String

data NegTest a = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _checkErr :: a -> Maybe FailMsg
  }

root :: FilePath
root = "tests/negative"

testDescr :: Typeable a => NegTest a -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint _file
            res <- runIOEither (upToAbstract entryPoint)
            case mapLeft fromMiniJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure "The scope checker did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not in the scoper."
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope negative tests"
    ( map (mkTest . testDescr) scoperErrorTests
        <> map (mkTest . testDescr) filesErrorTests
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

scoperErrorTests :: [NegTest ScoperError]
scoperErrorTests =
  [ NegTest
      "Not in scope"
      "."
      "NotInScope.mjuvix"
      $ \case
        ErrSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Qualified not in scope"
      "."
      "QualSymNotInScope.mjuvix"
      $ \case
        ErrQualSymNotInScope {} -> Nothing
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
        _ -> wrongError,
    NegTest
      "Wrong location of a compile block"
      "CompileBlocks"
      "WrongLocationCompileBlock.mjuvix"
      $ \case
        ErrWrongLocationCompileBlock {} -> Nothing
        _ -> wrongError,
    NegTest
      "Implicit argument on the left of an application"
      "."
      "AppLeftImplicit.mjuvix"
      $ \case
        ErrAppLeftImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple compile blocks for the same name"
      "CompileBlocks"
      "MultipleCompileBlockSameName.mjuvix"
      $ \case
        ErrMultipleCompileBlockSameName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple rules for a backend inside a compile block"
      "CompileBlocks"
      "MultipleCompileRuleSameBackend.mjuvix"
      $ \case
        ErrMultipleCompileRuleSameBackend {} -> Nothing
        _ -> wrongError,
    NegTest
      "issue 230"
      "230"
      "Prod.mjuvix"
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Compile block for a unsupported kind of expression"
      "CompileBlocks"
      "WrongKindExpressionCompileBlock.mjuvix"
      $ \case
        ErrWrongKindExpressionCompileBlock {} -> Nothing
        _ -> wrongError
  ]

filesErrorTests :: [NegTest FilesError]
filesErrorTests =
  [ NegTest
      "A module that conflicts with a module in the stdlib"
      "StdlibConflict"
      "Stdlib/Data/Bool.mjuvix"
      $ \case
        FilesError {} -> Nothing,
    NegTest
      "Importing a module that conflicts with a module in the stdlib"
      "StdlibConflict"
      "Input.mjuvix"
      $ \case
        FilesError {} -> Nothing
  ]
