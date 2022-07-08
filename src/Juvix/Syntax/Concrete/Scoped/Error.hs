module Juvix.Syntax.Concrete.Scoped.Error
  ( module Juvix.Syntax.Concrete.Scoped.Error.Types,
    module Juvix.Syntax.Concrete.Scoped.Error,
    module Juvix.Syntax.Concrete.Scoped.Error.Pretty,
  )
where

import Juvix.Prelude
import Juvix.Syntax.Concrete.Scoped.Error.Pretty
import Juvix.Syntax.Concrete.Scoped.Error.Types

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
  deriving stock (Show)

instance ToGenericError ScoperError where
  genericError = \case
    ErrParser e -> genericError e
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
    ErrWrongTopModuleName e -> genericError e
    ErrAmbiguousModuleSym e -> genericError e
    ErrUnusedOperatorDef e -> genericError e
    ErrLacksFunctionClause e -> genericError e
    ErrWrongLocationCompileBlock e -> genericError e
    ErrMultipleCompileBlockSameName e -> genericError e
    ErrMultipleCompileRuleSameBackend e -> genericError e
    ErrWrongKindExpressionCompileBlock e -> genericError e
