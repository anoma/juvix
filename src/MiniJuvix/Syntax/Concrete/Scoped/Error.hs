module MiniJuvix.Syntax.Concrete.Scoped.Error
  ( module MiniJuvix.Syntax.Concrete.Scoped.Error.Types,
    module MiniJuvix.Syntax.Concrete.Scoped.Error,
    module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import Prettyprinter
import Prettyprinter.Render.Text

-- | An error that happens during scope checking. Note that it is defined here
-- instead of in Error.Types to avoid orphan instances.
data ScopeError
  = ErrParser MegaParsecError
  | ErrInfixParser InfixError
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

ppScopeError :: ScopeError -> Doc Eann
ppScopeError s = case s of
  ErrParser txt -> ppError txt
  ErrInfixParser e -> ppError e
  ErrInfixPattern e -> ppError e
  ErrMultipleDeclarations e -> ppError e
  ErrLacksTypeSig e -> ppError e
  ErrImportCycle e -> ppError e
  ErrSymNotInScope e -> ppError e
  ErrQualSymNotInScope e -> ppError e
  ErrModuleNotInScope e -> ppError e
  ErrBindGroup e -> ppError e
  ErrDuplicateFixity e -> ppError e
  ErrMultipleExport e -> ppError e
  ErrAmbiguousSym e -> ppError e
  ErrWrongTopModuleName e -> ppError e
  ErrAmbiguousModuleSym e -> ppError e
  ErrUnusedOperatorDef e -> ppError e
  ErrLacksFunctionClause e -> ppError e
  ErrWrongLocationCompileBlock e -> ppError e
  ErrMultipleCompileBlockSameName e -> ppError e
  ErrMultipleCompileRuleSameBackend e -> ppError e
  ErrWrongKindExpressionCompileBlock e -> ppError e

genericError' :: ScopeError -> Maybe GenericError
genericError' = \case
  ErrParser e -> genericError e
  ErrInfixParser {} -> Nothing
  ErrInfixPattern {} -> Nothing
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

docStream :: ScopeError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppScopeError

instance ToGenericError ScopeError where
  genericError = genericError'

instance JuvixError ScopeError where
  renderText :: ScopeError -> Text
  renderText = renderStrict . docStream

  renderAnsiText = renderStrict . docStream
