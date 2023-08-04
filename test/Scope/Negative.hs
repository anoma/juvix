module Scope.Negative (allTests) where

import Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types (RepeatedField(RepeatedField))

type FailMsg = String

data NegTest a = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: a -> Maybe FailMsg
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

testDescr :: (Typeable a) => NegTest a -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- defaultEntryPointCwdIO file'
            res <- runIOEither entryPoint upToInternal
            case mapLeft fromJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure "An error ocurred but it was not in the scoper."
              Right {} -> assertFailure "The scope checker did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope negative tests"
    ( map (mkTest . testDescr) scoperErrorTests
    )

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

scoperErrorTests :: [NegTest ScoperError]
scoperErrorTests =
  [ NegTest
      "Not in scope"
      $(mkRelDir ".")
      $(mkRelFile "NotInScope.juvix")
      $ \case
        ErrSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Qualified not in scope"
      $(mkRelDir ".")
      $(mkRelFile "QualSymNotInScope.juvix")
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple declarations"
      $(mkRelDir ".")
      $(mkRelFile "MultipleDeclarations.juvix")
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Import cycle"
      $(mkRelDir "ImportCycle")
      $(mkRelFile "A.juvix")
      $ \case
        ErrImportCycle {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (function clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Clause.juvix")
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Binding group conflict (lambda clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Lambda.juvix")
      $ \case
        ErrMultipleDeclarations {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (expression)"
      $(mkRelDir ".")
      $(mkRelFile "InfixError.juvix")
      $ \case
        ErrInfixParser {} -> Nothing
        _ -> wrongError,
    NegTest
      "Infix error (pattern)"
      $(mkRelDir ".")
      $(mkRelFile "InfixErrorP.juvix")
      $ \case
        ErrInfixPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Duplicate fixity declaration"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateFixity.juvix")
      $ \case
        ErrDuplicateFixity {} -> Nothing
        _ -> wrongError,
    NegTest
      "Multiple export conflict"
      $(mkRelDir ".")
      $(mkRelFile "MultipleExportConflict.juvix")
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Module not in scope"
      $(mkRelDir ".")
      $(mkRelFile "ModuleNotInScope.juvix")
      $ \case
        ErrModuleNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unused operator syntax definition"
      $(mkRelDir ".")
      $(mkRelFile "UnusedOperatorDef.juvix")
      $ \case
        ErrUnusedOperatorDef {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous symbol"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousSymbol.juvix")
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause"
      $(mkRelDir ".")
      $(mkRelFile "LacksFunctionClause.juvix")
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Lacks function clause inside let"
      $(mkRelDir ".")
      $(mkRelFile "LetMissingClause.juvix")
      $ \case
        ErrLacksFunctionClause {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous export"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousExport.juvix")
      $ \case
        ErrMultipleExport {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested modules"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousModule.juvix")
      $ \case
        ErrAmbiguousModuleSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Ambiguous nested constructors"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousConstructor.juvix")
      $ \case
        ErrAmbiguousSym {} -> Nothing
        _ -> wrongError,
    NegTest
      "Implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "AppLeftImplicit.juvix")
      $ \case
        ErrAppLeftImplicit {} -> Nothing
        _ -> wrongError,
    NegTest
      "issue 230"
      $(mkRelDir "230")
      $(mkRelFile "Prod.juvix")
      $ \case
        ErrQualSymNotInScope {} -> Nothing
        _ -> wrongError,
    NegTest
      "Double braces in pattern"
      $(mkRelDir ".")
      $(mkRelFile "NestedPatternBraces.juvix")
      $ \case
        ErrDoubleBracesPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "As-Pattern aliasing variable"
      $(mkRelDir ".")
      $(mkRelFile "AsPatternAlias.juvix")
      $ \case
        ErrAliasBinderPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Nested As-Patterns"
      $(mkRelDir ".")
      $(mkRelFile "NestedAsPatterns.juvix")
      $ \case
        ErrDoubleBinderPattern {} -> Nothing
        _ -> wrongError,
    NegTest
      "Pattern matching an implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "ImplicitPatternLeftApplication.juvix")
      $ \case
        ErrImplicitPatternLeftApplication {} -> Nothing
        _ -> wrongError,
    NegTest
      "Constructor expected on the left of a pattern application"
      $(mkRelDir ".")
      $(mkRelFile "ConstructorExpectedLeftApplication.juvix")
      $ \case
        ErrConstructorExpectedLeftApplication {} -> Nothing
        _ -> wrongError,
    NegTest
      "A type parameter name occurs twice when declaring an inductive type"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateInductiveParameterName.juvix")
      $ \case
        ErrNameSignature (ErrDuplicateName DuplicateName {}) -> Nothing
        _ -> wrongError,
    NegTest
      "A function lacks a type signature"
      $(mkRelDir ".")
      $(mkRelFile "LacksTypeSig.juvix")
      $ \case
        ErrLacksTypeSig {} -> Nothing
        _ -> wrongError,
    NegTest
      "A function inside a let lacks a type signature that is at the top level"
      $(mkRelDir ".")
      $(mkRelFile "LacksTypeSig2.juvix")
      $ \case
        ErrLacksTypeSig {} -> Nothing
        _ -> wrongError,
    NegTest
      "Using symbol that is not exported"
      $(mkRelDir "UsingHiding")
      $(mkRelFile "Main.juvix")
      $ \case
        ErrModuleDoesNotExportSymbol {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong number of interator initializers"
      $(mkRelDir ".")
      $(mkRelFile "Iterators1.juvix")
      $ \case
        ErrIteratorInitializer {} -> Nothing
        _ -> wrongError,
    NegTest
      "Wrong number of interator ranges"
      $(mkRelDir ".")
      $(mkRelFile "Iterators2.juvix")
      $ \case
        ErrIteratorRange {} -> Nothing
        _ -> wrongError,
    NegTest
      "Undeclared iterator"
      $(mkRelDir ".")
      $(mkRelFile "Iterators3.juvix")
      $ \case
        ErrIteratorUndefined {} -> Nothing
        _ -> wrongError,
    NegTest
      "Duplicate iterator declaration"
      $(mkRelDir ".")
      $(mkRelFile "Iterators4.juvix")
      $ \case
        ErrDuplicateIterator {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unused iterator declaration"
      $(mkRelDir ".")
      $(mkRelFile "Iterators5.juvix")
      $ \case
        ErrUnusedIteratorDef {} -> Nothing
        _ -> wrongError,
    NegTest
      "Repeated name in named application"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateArgument.juvix")
      $ \case
        ErrNamedArgumentsError (ErrDuplicateArgument {}) -> Nothing
        _ -> wrongError,
    NegTest
      "Unexpected named argument after wildcard"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedArgumentWildcard.juvix")
      $ \case
        ErrNamedArgumentsError ErrUnexpectedArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unexpected named argument"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedArgument.juvix")
      $ \case
        ErrNamedArgumentsError ErrUnexpectedArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Missing argument"
      $(mkRelDir ".")
      $(mkRelFile "MissingArgument.juvix")
      $ \case
        ErrNamedArgumentsError ErrMissingArguments {} -> Nothing
        _ -> wrongError,
    NegTest
      "Repeated name in name signature"
      $(mkRelDir ".")
      $(mkRelFile "RepeatedNameSignature.juvix")
      $ \case
        ErrNameSignature ErrDuplicateName {} -> Nothing
        _ -> wrongError,
    NegTest
      "No named arguments"
      $(mkRelDir ".")
      $(mkRelFile "NoNamedArguments.juvix")
      $ \case
        ErrNoNameSignature NoNameSignature {} -> Nothing
        _ -> wrongError,
    NegTest
      "Not a record"
      $(mkRelDir ".")
      $(mkRelFile "NotARecord.juvix")
      $ \case
        ErrNotARecord NotARecord {} -> Nothing
        _ -> wrongError,
    NegTest
      "Unexpected field in record update"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedFieldUpdate.juvix")
      $ \case
        ErrUnexpectedField UnexpectedField {} -> Nothing
        _ -> wrongError,
    NegTest
      "Repeated field in record pattern"
      $(mkRelDir ".")
      $(mkRelFile "RepeatedFieldPattern.juvix")
      $ \case
        ErrRepeatedField RepeatedField {} -> Nothing
        _ -> wrongError
  ]
