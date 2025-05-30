module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
    module Juvix.Compiler.Concrete.Data.NameSignature.Error,
    module Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments.Error,
  )
where

import Juvix.Compiler.Concrete.Data.NameSignature.Error
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types
import Juvix.Compiler.Internal.Translation.FromConcrete.NamedArguments.Error
import Juvix.Prelude

data ScoperError
  = ErrInfixParser InfixError
  | ErrAppLeftImplicit AppLeftImplicit
  | ErrDanglingDoubleBrace DanglingDoubleBrace
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrImportCycle ImportCycle
  | ErrImportCycleNew ImportCycleNew
  | ErrSymNotInScope NotInScope
  | ErrQualSymNotInScope QualSymNotInScope
  | ErrModuleNotInScope ModuleNotInScope
  | ErrMultipleExport MultipleExportConflict
  | ErrAmbiguousSym AmbiguousSym
  | ErrAmbiguousModuleSym AmbiguousModuleSym
  | ErrDoubleBracesPattern DoubleBracesPattern
  | ErrDoubleBinderPattern DoubleBinderPattern
  | ErrAliasBinderPattern AliasBinderPattern
  | ErrImplicitPatternLeftApplication ImplicitPatternLeftApplication
  | ErrConstructorExpectedLeftApplication ConstructorExpectedLeftApplication
  | ErrModuleDoesNotExportSymbol ModuleDoesNotExportSymbol
  | ErrIteratorInitializer IteratorInitializer
  | ErrIteratorRange IteratorRange
  | ErrIteratorUndefined IteratorUndefined
  | ErrNameSignature NameSignatureError
  | ErrNoNameSignature NoNameSignature
  | ErrNamedArgumentsError NamedArgumentsError
  | ErrNotARecord NotARecord
  | ErrUnexpectedArgument UnexpectedArgument
  | ErrUnexpectedField UnexpectedField
  | ErrRepeatedField RepeatedField
  | ErrConstructorNotARecord ConstructorNotARecord
  | ErrNotAConstructor NotAConstructor
  | ErrMissingArgs MissingArgs
  | ErrPrecedenceInconsistency PrecedenceInconsistencyError
  | ErrInvalidRangeNumber InvalidRangeNumber
  | ErrWrongDefaultValue WrongDefaultValue
  | ErrUnsupported Unsupported
  | ErrDefaultArgCycle DefaultArgCycle
  | ErrBuiltinAlreadyDefined BuiltinAlreadyDefined
  | ErrBuiltinNotDefined BuiltinNotDefined
  | ErrBuiltinErrorMessage BuiltinErrorMessage
  | ErrDoLastStatement DoLastStatement
  | ErrDoBindImplicitPattern DoBindImplicitPattern
  | ErrDerivingTypeWrongForm DerivingTypeWrongForm
  deriving stock (Generic)

instance ToGenericError ScoperError where
  genericError = \case
    ErrInfixParser e -> genericError e
    ErrAppLeftImplicit e -> genericError e
    ErrDanglingDoubleBrace e -> genericError e
    ErrInfixPattern e -> genericError e
    ErrMultipleDeclarations e -> genericError e
    ErrImportCycle e -> genericError e
    ErrImportCycleNew e -> genericError e
    ErrSymNotInScope e -> genericError e
    ErrQualSymNotInScope e -> genericError e
    ErrModuleNotInScope e -> genericError e
    ErrMultipleExport e -> genericError e
    ErrAmbiguousSym e -> genericError e
    ErrAmbiguousModuleSym e -> genericError e
    ErrDoubleBracesPattern e -> genericError e
    ErrDoubleBinderPattern e -> genericError e
    ErrAliasBinderPattern e -> genericError e
    ErrImplicitPatternLeftApplication e -> genericError e
    ErrConstructorExpectedLeftApplication e -> genericError e
    ErrModuleDoesNotExportSymbol e -> genericError e
    ErrIteratorInitializer e -> genericError e
    ErrIteratorRange e -> genericError e
    ErrIteratorUndefined e -> genericError e
    ErrNameSignature e -> genericError e
    ErrNoNameSignature e -> genericError e
    ErrNamedArgumentsError e -> genericError e
    ErrNotARecord e -> genericError e
    ErrUnexpectedArgument e -> genericError e
    ErrUnexpectedField e -> genericError e
    ErrRepeatedField e -> genericError e
    ErrConstructorNotARecord e -> genericError e
    ErrNotAConstructor e -> genericError e
    ErrMissingArgs e -> genericError e
    ErrPrecedenceInconsistency e -> genericError e
    ErrInvalidRangeNumber e -> genericError e
    ErrWrongDefaultValue e -> genericError e
    ErrUnsupported e -> genericError e
    ErrDefaultArgCycle e -> genericError e
    ErrBuiltinAlreadyDefined e -> genericError e
    ErrBuiltinNotDefined e -> genericError e
    ErrBuiltinErrorMessage e -> genericError e
    ErrDoLastStatement e -> genericError e
    ErrDoBindImplicitPattern e -> genericError e
    ErrDerivingTypeWrongForm e -> genericError e

builtinsErrorMsg :: (Members '[Error ScoperError] r) => Interval -> AnsiText -> Sem r a
builtinsErrorMsg loc msg =
  throw $
    ErrBuiltinErrorMessage
      BuiltinErrorMessage
        { _builtinErrorMessageLoc = loc,
          _builtinErrorMessage = msg
        }

builtinsErrorText :: (Members '[Error ScoperError] r) => Interval -> Text -> Sem r a
builtinsErrorText loc = builtinsErrorMsg loc . mkAnsiText
