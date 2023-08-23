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

data ScoperError
  = ErrInfixParser InfixError
  | ErrAppLeftImplicit AppLeftImplicit
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrImportCycle ImportCycle
  | ErrSymNotInScope NotInScope
  | ErrQualSymNotInScope QualSymNotInScope
  | ErrModuleNotInScope ModuleNotInScope
  | ErrDuplicateOperator DuplicateOperator
  | ErrDuplicateIterator DuplicateIterator
  | ErrMultipleExport MultipleExportConflict
  | ErrAmbiguousSym AmbiguousSym
  | ErrAmbiguousModuleSym AmbiguousModuleSym
  | ErrUnusedOperatorDef UnusedOperatorDef
  | ErrUnusedIteratorDef UnusedIteratorDef
  | ErrDoubleBracesPattern DoubleBracesPattern
  | ErrDoubleBinderPattern DoubleBinderPattern
  | ErrAliasBinderPattern AliasBinderPattern
  | ErrImplicitPatternLeftApplication ImplicitPatternLeftApplication
  | ErrConstructorExpectedLeftApplication ConstructorExpectedLeftApplication
  | ErrCaseBranchImplicitPattern CaseBranchImplicitPattern
  | ErrModuleDoesNotExportSymbol ModuleDoesNotExportSymbol
  | ErrIteratorInitializer IteratorInitializer
  | ErrIteratorRange IteratorRange
  | ErrIteratorUndefined IteratorUndefined
  | ErrNameSignature NameSignatureError
  | ErrNoNameSignature NoNameSignature
  | ErrNamedArgumentsError NamedArgumentsError
  | ErrNotARecord NotARecord
  | ErrUnexpectedField UnexpectedField
  | ErrRepeatedField RepeatedField
  | ErrConstructorNotARecord ConstructorNotARecord
  | ErrPrecedenceInconsistency PrecedenceInconsistencyError
  | ErrIncomparablePrecedences IncomaprablePrecedences
  | ErrAliasCycle AliasCycle

instance ToGenericError ScoperError where
  genericError = \case
    ErrCaseBranchImplicitPattern e -> genericError e
    ErrInfixParser e -> genericError e
    ErrAppLeftImplicit e -> genericError e
    ErrInfixPattern e -> genericError e
    ErrMultipleDeclarations e -> genericError e
    ErrImportCycle e -> genericError e
    ErrSymNotInScope e -> genericError e
    ErrQualSymNotInScope e -> genericError e
    ErrModuleNotInScope e -> genericError e
    ErrDuplicateOperator e -> genericError e
    ErrDuplicateIterator e -> genericError e
    ErrMultipleExport e -> genericError e
    ErrAmbiguousSym e -> genericError e
    ErrAmbiguousModuleSym e -> genericError e
    ErrUnusedOperatorDef e -> genericError e
    ErrUnusedIteratorDef e -> genericError e
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
    ErrUnexpectedField e -> genericError e
    ErrRepeatedField e -> genericError e
    ErrConstructorNotARecord e -> genericError e
    ErrPrecedenceInconsistency e -> genericError e
    ErrIncomparablePrecedences e -> genericError e
    ErrAliasCycle e -> genericError e
