module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
  ( module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
    -- ,
    -- module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty,
  )
where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types
import Juvix.Prelude

data ScoperError
  = ErrInfixParser InfixError
  | ErrAppLeftImplicit AppLeftImplicit
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrLacksTypeSig LacksTypeSig
  | ErrLacksFunctionClause LacksFunctionClause
  | ErrDuplicateFunctionClause DuplicateFunctionClause
  | ErrImportCycle ImportCycle
  | ErrSymNotInScope NotInScope
  | ErrQualSymNotInScope QualSymNotInScope
  | ErrModuleNotInScope ModuleNotInScope
  | ErrDuplicateFixity DuplicateFixity
  | ErrMultipleExport MultipleExportConflict
  | ErrAmbiguousSym AmbiguousSym
  | ErrAmbiguousModuleSym AmbiguousModuleSym
  | ErrUnusedOperatorDef UnusedOperatorDef
  | ErrDoubleBracesPattern DoubleBracesPattern
  | ErrDoubleBinderPattern DoubleBinderPattern
  | ErrAliasBinderPattern AliasBinderPattern
  | ErrImplicitPatternLeftApplication ImplicitPatternLeftApplication
  | ErrConstructorExpectedLeftApplication ConstructorExpectedLeftApplication
  | ErrCaseBranchImplicitPattern CaseBranchImplicitPattern
  | ErrModuleDoesNotExportSymbol ModuleDoesNotExportSymbol
  deriving stock (Show)

instance ToGenericError ScoperError where
  genericError = \case
    ErrCaseBranchImplicitPattern e -> genericError e
    ErrDuplicateFunctionClause e -> genericError e
    ErrInfixParser e -> genericError e
    ErrAppLeftImplicit e -> genericError e
    ErrInfixPattern e -> genericError e
    ErrMultipleDeclarations e -> genericError e
    ErrLacksTypeSig e -> genericError e
    ErrImportCycle e -> genericError e
    ErrSymNotInScope e -> genericError e
    ErrQualSymNotInScope e -> genericError e
    ErrModuleNotInScope e -> genericError e
    ErrDuplicateFixity e -> genericError e
    ErrMultipleExport e -> genericError e
    ErrAmbiguousSym e -> genericError e
    ErrAmbiguousModuleSym e -> genericError e
    ErrUnusedOperatorDef e -> genericError e
    ErrLacksFunctionClause e -> genericError e
    ErrDoubleBracesPattern e -> genericError e
    ErrDoubleBinderPattern e -> genericError e
    ErrAliasBinderPattern e -> genericError e
    ErrImplicitPatternLeftApplication e -> genericError e
    ErrConstructorExpectedLeftApplication e -> genericError e
    ErrModuleDoesNotExportSymbol e -> genericError e
