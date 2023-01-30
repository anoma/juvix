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
  | ErrBindGroup BindGroupConflict
  | ErrDuplicateFixity DuplicateFixity
  | ErrMultipleExport MultipleExportConflict
  | ErrAmbiguousSym AmbiguousSym
  | ErrAmbiguousModuleSym AmbiguousModuleSym
  | ErrUnusedOperatorDef UnusedOperatorDef
  | ErrWrongLocationCompileBlock WrongLocationCompileBlock
  | ErrMultipleCompileBlockSameName MultipleCompileBlockSameName
  | ErrMultipleCompileRuleSameBackend MultipleCompileRuleSameBackend
  | ErrWrongKindExpressionCompileBlock WrongKindExpressionCompileBlock
  | ErrDuplicateInductiveParameterName DuplicateInductiveParameterName
  | ErrDoubleBracesPattern DoubleBracesPattern
  | ErrDoubleBinderPattern DoubleBinderPattern
  | ErrAliasBinderPattern AliasBinderPattern
  | ErrImplicitPatternLeftApplication ImplicitPatternLeftApplication
  | ErrConstructorExpectedLeftApplication ConstructorExpectedLeftApplication
  deriving stock (Show)

instance ToGenericError ScoperError where
  genericError = \case
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
    ErrBindGroup e -> genericError e
    ErrDuplicateFixity e -> genericError e
    ErrMultipleExport e -> genericError e
    ErrAmbiguousSym e -> genericError e
    ErrAmbiguousModuleSym e -> genericError e
    ErrUnusedOperatorDef e -> genericError e
    ErrLacksFunctionClause e -> genericError e
    ErrWrongLocationCompileBlock e -> genericError e
    ErrMultipleCompileBlockSameName e -> genericError e
    ErrMultipleCompileRuleSameBackend e -> genericError e
    ErrWrongKindExpressionCompileBlock e -> genericError e
    ErrDuplicateInductiveParameterName e -> genericError e
    ErrDoubleBracesPattern e -> genericError e
    ErrDoubleBinderPattern e -> genericError e
    ErrAliasBinderPattern e -> genericError e
    ErrImplicitPatternLeftApplication e -> genericError e
    ErrConstructorExpectedLeftApplication e -> genericError e
