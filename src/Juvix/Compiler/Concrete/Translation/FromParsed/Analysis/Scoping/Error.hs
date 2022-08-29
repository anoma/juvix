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
  = ErrParser MegaParsecError
  | ErrInfixParser InfixError
  | ErrAppLeftImplicit AppLeftImplicit
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrLacksTypeSig LacksTypeSig
  | ErrLacksFunctionClause LacksFunctionClause
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
  | ErrWrongTopModuleName WrongTopModuleName
  | ErrWrongLocationCompileBlock WrongLocationCompileBlock
  | ErrMultipleCompileBlockSameName MultipleCompileBlockSameName
  | ErrMultipleCompileRuleSameBackend MultipleCompileRuleSameBackend
  | ErrWrongKindExpressionCompileBlock WrongKindExpressionCompileBlock
  | ErrDuplicateInductiveParameterName DuplicateInductiveParameterName
  | ErrDoubleBracesPattern DoubleBracesPattern
  | ErrImplicitPatternLeftApplication ImplicitPatternLeftApplication
  | ErrConstructorExpectedLeftApplication ConstructorExpectedLeftApplication
  deriving stock (Show)

instance ToGenericError ScoperError where
  genericError opts = \case
    ErrParser e -> genericError opts e
    ErrInfixParser e -> genericError opts e
    ErrAppLeftImplicit e -> genericError opts e
    ErrInfixPattern e -> genericError opts e
    ErrMultipleDeclarations e -> genericError opts e
    ErrLacksTypeSig e -> genericError opts e
    ErrImportCycle e -> genericError opts e
    ErrSymNotInScope e -> genericError opts e
    ErrQualSymNotInScope e -> genericError opts e
    ErrModuleNotInScope e -> genericError opts e
    ErrBindGroup e -> genericError opts e
    ErrDuplicateFixity e -> genericError opts e
    ErrMultipleExport e -> genericError opts e
    ErrAmbiguousSym e -> genericError opts e
    ErrWrongTopModuleName e -> genericError opts e
    ErrAmbiguousModuleSym e -> genericError opts e
    ErrUnusedOperatorDef e -> genericError opts e
    ErrLacksFunctionClause e -> genericError opts e
    ErrWrongLocationCompileBlock e -> genericError opts e
    ErrMultipleCompileBlockSameName e -> genericError opts e
    ErrMultipleCompileRuleSameBackend e -> genericError opts e
    ErrWrongKindExpressionCompileBlock e -> genericError opts e
    ErrDuplicateInductiveParameterName e -> genericError opts e
    ErrDoubleBracesPattern e -> genericError opts e
    ErrImplicitPatternLeftApplication e -> genericError opts e
    ErrConstructorExpectedLeftApplication e -> genericError opts e
