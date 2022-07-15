module Scope.Negative (allTests) where

import Base
import Juvix.Pipeline
import Juvix.Syntax.Concrete.Scoped.Error

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
            case mapLeft fromJuvixError res of
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
      "NotInScope.juvix"
      $ \case
        ErrSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Qualified not in scope"
      "."
      "QualSymNotInScope.juvix"
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple declarations"
      "."
      "MultipleDeclarations.juvix"
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Import cycle"
      "ImportCycle"
      "A.juvix"
      $ \case
        ErrImportCycle {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (function clause)"
      "BindGroupConflict"
      "Clause.juvix"
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (lambda clause)"
      "BindGroupConflict"
      "Lambda.juvix"
      $ \case
        ErrBindGroup {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (expression)"
      "."
      "InfixError.juvix"
      $ \case
        ErrInfixParser {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (pattern)"
      "."
      "InfixErrorP.juvix"
      $ \case
        ErrInfixPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Duplicate fixity declaration"
      "."
      "DuplicateFixity.juvix"
      $ \case
        ErrDuplicateFixity {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple export conflict"
      "."
      "MultipleExportConflict.juvix"
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Module not in scope"
      "."
      "ModuleNotInScope.juvix"
      $ \case
        ErrModuleNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unused operator syntax definition"
      "."
      "UnusedOperatorDef.juvix"
      $ \case
        ErrUnusedOperatorDef {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous symbol"
      "."
      "AmbiguousSymbol.juvix"
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause"
      "."
      "LacksFunctionClause.juvix"
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Incorrect top module path"
      "."
      "WrongModuleName.juvix"
      $ \case
        ErrWrongTopModuleName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous export"
      "."
      "AmbiguousExport.juvix"
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested modules"
      "."
      "AmbiguousModule.juvix"
      $ \case
        ErrAmbiguousModuleSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested constructors"
      "."
      "AmbiguousConstructor.juvix"
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong location of a compile block"
      "CompileBlocks"
      "WrongLocationCompileBlock.juvix"
      $ \case
        ErrWrongLocationCompileBlock {} -> Nothing
        _ -> wrongError,
    NegTest
      "Implicit argument on the left of an application"
      "."
      "AppLeftImplicit.juvix"
      $ \case
        ErrAppLeftImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple compile blocks for the same name"
      "CompileBlocks"
      "MultipleCompileBlockSameName.juvix"
      $ \case
        ErrMultipleCompileBlockSameName {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple rules for a backend inside a compile block"
      "CompileBlocks"
      "MultipleCompileRuleSameBackend.juvix"
      $ \case
        ErrMultipleCompileRuleSameBackend {} -> Nothing
        _ -> wrongError,
    NegTest
      "issue 230"
      "230"
      "Prod.juvix"
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Compile block for a unsupported kind of expression"
      "CompileBlocks"
      "WrongKindExpressionCompileBlock.juvix"
      $ \case
        ErrWrongKindExpressionCompileBlock {} -> Nothing
        _ -> wrongError,
    NegTest
      "A type parameter name occurs twice when declaring an inductive type"
      "."
      "DuplicateInductiveParameterName.juvix"
      $ \case
        ErrDuplicateInductiveParameterName {} -> Nothing
        _ -> wrongError
  ]

filesErrorTests :: [NegTest FilesError]
filesErrorTests =
  [ NegTest
      "A module that conflicts with a module in the stdlib"
      "StdlibConflict"
      "Stdlib/Data/Bool.juvix"
      $ \case
        FilesError {} -> Nothing,
    NegTest
      "Importing a module that conflicts with a module in the stdlib"
      "StdlibConflict"
      "Input.juvix"
      $ \case
        FilesError {} -> Nothing
  ]
