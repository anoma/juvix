module MiniJuvix.Syntax.Concrete.Scoped.Error
  ( module MiniJuvix.Syntax.Concrete.Scoped.Error.Types,
    module MiniJuvix.Syntax.Concrete.Scoped.Error,
    module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty qualified as P
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import Prettyprinter

-- | An error that happens during scope checking. Note that it is defined here
-- instead of in ./Error/Types to avoid orphan instances.
data ScopeError
  = ErrParser MegaParsecError
  | ErrInfixParser InfixError
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrLacksTypeSig LacksTypeSig
  | ErrLacksFunctionClause LacksFunctionClause
  | ErrImportCycle ImportCycle
  | ErrSymNotInScope NotInScope
  | ErrQualSymNotInScope QualifiedName
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
  ErrQualSymNotInScope {} -> pretty (show s :: Text)
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

docStream :: ScopeError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppScopeError

instance JuvixError ScopeError where
  renderAnsiText :: ScopeError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: ScopeError -> Text
  renderText = P.renderText . docStream
