module Scope.Negative (allTests) where

import Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error

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
            entryPoint <- testDefaultEntryPointIO tRoot file'
            res <- testRunIOEitherTermination entryPoint upToInternal
            case mapLeft fromJuvixError res of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Left Nothing -> assertFailure ("An error occurred but it was not in the scoper.\nFile: " <> prettyString file')
              Right {} -> assertFailure "The scope checker did not find an error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope negative tests"
    ( map (mkTest . testDescr) scoperErrorTests
    )

negTest ::
  String ->
  Path Rel Dir ->
  Path Rel File ->
  (Path Abs File -> ScoperError -> Maybe FailMsg) ->
  NegTest ScoperError
negTest tname rdir rfile chk =
  NegTest
    { _name = tname,
      _relDir = rdir,
      _file = rfile,
      _checkErr = chk (root <//> rdir <//> rfile)
    }

scoperErrorTests :: [NegTest ScoperError]
scoperErrorTests =
  [ negTest
      "Not in scope"
      $(mkRelDir ".")
      $(mkRelFile "NotInScope.juvix")
      $ wantsError ErrSymNotInScope,
    negTest
      "Qualified not in scope"
      $(mkRelDir ".")
      $(mkRelFile "QualSymNotInScope.juvix")
      $ wantsError ErrQualSymNotInScope,
    negTest
      "Multiple declarations"
      $(mkRelDir ".")
      $(mkRelFile "MultipleDeclarations.juvix")
      $ wantsError ErrMultipleDeclarations,
    negTest
      "Import cycle"
      $(mkRelDir "ImportCycle")
      $(mkRelFile "A.juvix")
      $ wantsError ErrImportCycleNew,
    negTest
      "Binding group conflict (function clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Clause.juvix")
      $ wantsError ErrMultipleDeclarations,
    negTest
      "Binding group conflict (lambda clause)"
      $(mkRelDir "BindGroupConflict")
      $(mkRelFile "Lambda.juvix")
      $ wantsError ErrMultipleDeclarations,
    negTest
      "Infix error (expression)"
      $(mkRelDir ".")
      $(mkRelFile "InfixError.juvix")
      $ wantsError ErrInfixParser,
    negTest
      "Infix error (pattern)"
      $(mkRelDir ".")
      $(mkRelFile "InfixErrorP.juvix")
      $ wantsError ErrInfixPattern,
    negTest
      "Duplicate operator declaration"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateOperator.juvix")
      $ wantsError ErrDuplicateOperator,
    negTest
      "Multiple export conflict"
      $(mkRelDir ".")
      $(mkRelFile "MultipleExportConflict.juvix")
      $ wantsError ErrMultipleExport,
    negTest
      "Module not in scope"
      $(mkRelDir ".")
      $(mkRelFile "ModuleNotInScope.juvix")
      $ wantsError ErrModuleNotInScope,
    negTest
      "Unused operator syntax definition"
      $(mkRelDir ".")
      $(mkRelFile "UnusedOperatorDef.juvix")
      $ wantsError ErrUnusedOperatorDef,
    negTest
      "Ambiguous symbol"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousSymbol.juvix")
      $ wantsError ErrAmbiguousSym,
    negTest
      "Ambiguous export"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousExport.juvix")
      $ wantsError ErrMultipleExport,
    negTest
      "Ambiguous nested modules"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousModule.juvix")
      $ wantsError ErrAmbiguousModuleSym,
    negTest
      "Ambiguous nested constructors"
      $(mkRelDir ".")
      $(mkRelFile "AmbiguousConstructor.juvix")
      $ wantsError ErrAmbiguousSym,
    negTest
      "Implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "AppLeftImplicit.juvix")
      $ wantsError ErrAppLeftImplicit,
    negTest
      "issue 230"
      $(mkRelDir "230")
      $(mkRelFile "Prod.juvix")
      $ wantsError ErrQualSymNotInScope,
    negTest
      "Double braces in pattern"
      $(mkRelDir ".")
      $(mkRelFile "NestedPatternBraces.juvix")
      $ wantsError ErrDoubleBracesPattern,
    negTest
      "As-Pattern aliasing variable"
      $(mkRelDir ".")
      $(mkRelFile "AsPatternAlias.juvix")
      $ wantsError ErrAliasBinderPattern,
    negTest
      "Nested As-Patterns"
      $(mkRelDir ".")
      $(mkRelFile "NestedAsPatterns.juvix")
      $ wantsError ErrDoubleBinderPattern,
    negTest
      "Pattern matching an implicit argument on the left of an application"
      $(mkRelDir ".")
      $(mkRelFile "ImplicitPatternLeftApplication.juvix")
      $ wantsError ErrImplicitPatternLeftApplication,
    negTest
      "Constructor expected on the left of a pattern application"
      $(mkRelDir ".")
      $(mkRelFile "ConstructorExpectedLeftApplication.juvix")
      $ wantsError ErrConstructorExpectedLeftApplication,
    negTest
      "A type parameter name occurs twice when declaring an inductive type"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateInductiveParameterName.juvix")
      $ wantsError ErrNameSignature,
    negTest
      "Using symbol that is not exported"
      $(mkRelDir "UsingHiding")
      $(mkRelFile "Main.juvix")
      $ wantsError ErrModuleDoesNotExportSymbol,
    negTest
      "Wrong number of iterator initializers"
      $(mkRelDir ".")
      $(mkRelFile "Iterators1.juvix")
      $ wantsError ErrIteratorInitializer,
    negTest
      "Wrong number of iterator ranges"
      $(mkRelDir ".")
      $(mkRelFile "Iterators2.juvix")
      $ wantsError ErrIteratorRange,
    negTest
      "Undeclared iterator"
      $(mkRelDir ".")
      $(mkRelFile "Iterators3.juvix")
      $ wantsError ErrIteratorUndefined,
    negTest
      "Duplicate iterator declaration"
      $(mkRelDir ".")
      $(mkRelFile "Iterators4.juvix")
      $ wantsError ErrDuplicateIterator,
    negTest
      "Unused iterator declaration"
      $(mkRelDir ".")
      $(mkRelFile "Iterators5.juvix")
      $ wantsError ErrUnusedIteratorDef,
    negTest
      "Repeated name in named application"
      $(mkRelDir ".")
      $(mkRelFile "DuplicateArgument.juvix")
      $ wantsError ErrMultipleDeclarations,
    negTest
      "Unexpected named argument after wildcard"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedArgumentWildcard.juvix")
      $ wantsError ErrUnexpectedArgument,
    negTest
      "Unexpected named argument"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedArgument.juvix")
      $ wantsError ErrUnexpectedArgument,
    negTest
      "Missing argument"
      $(mkRelDir ".")
      $(mkRelFile "MissingArgument.juvix")
      $ wantsError ErrNamedArgumentsError,
    negTest
      "Repeated name in name signature"
      $(mkRelDir ".")
      $(mkRelFile "RepeatedNameSignature.juvix")
      $ wantsError ErrNameSignature,
    negTest
      "No named arguments"
      $(mkRelDir ".")
      $(mkRelFile "NoNamedArguments.juvix")
      $ wantsError ErrNoNameSignature,
    negTest
      "Not a record"
      $(mkRelDir ".")
      $(mkRelFile "NotARecord.juvix")
      $ wantsError ErrNotARecord,
    negTest
      "Unexpected field in record update"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedFieldUpdate.juvix")
      $ wantsError ErrUnexpectedField,
    negTest
      "Repeated field in record pattern"
      $(mkRelDir ".")
      $(mkRelFile "RepeatedFieldPattern.juvix")
      $ wantsError ErrRepeatedField,
    negTest
      "Missing fields in record creation"
      $(mkRelDir ".")
      $(mkRelFile "MissingFields.juvix")
      $ wantsError ErrMissingArgs,
    negTest
      "Unexpected argument"
      $(mkRelDir ".")
      $(mkRelFile "UnexpectedArgumentNew.juvix")
      $ wantsError ErrUnexpectedArgument,
    negTest
      "Incomparable precedences"
      $(mkRelDir ".")
      $(mkRelFile "IncomparablePrecedences.juvix")
      $ wantsError ErrIncomparablePrecedences,
    negTest
      "Precedence inconsistency"
      $(mkRelDir ".")
      $(mkRelFile "PrecedenceInconsistency.juvix")
      $ wantsError ErrPrecedenceInconsistency,
    negTest
      "Alias cycle"
      $(mkRelDir ".")
      $(mkRelFile "AliasCycle.juvix")
      $ wantsError ErrAliasCycle,
    negTest
      "Invalid range number in iterator definition"
      $(mkRelDir ".")
      $(mkRelFile "InvalidRangeNumber.juvix")
      $ wantsError ErrInvalidRangeNumber,
    negTest
      "Dangling double brace"
      $(mkRelDir "Internal")
      $(mkRelFile "DanglingDoubleBrace.juvix")
      $ wantsError ErrDanglingDoubleBrace,
    negTest
      "Nested let open shadowing"
      $(mkRelDir ".")
      $(mkRelFile "LetOpen.juvix")
      $ wantsError ErrAmbiguousSym,
    negTest
      "Invalid default"
      $(mkRelDir ".")
      $(mkRelFile "InvalidDefault.juvix")
      $ wantsError ErrWrongDefaultValue
  ]
