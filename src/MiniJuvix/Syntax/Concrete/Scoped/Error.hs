module MiniJuvix.Syntax.Concrete.Scoped.Error (
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Types,
  module MiniJuvix.Syntax.Concrete.Scoped.Error,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty
  ) where

import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty
import qualified MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty as P
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import Prettyprinter

-- | An error that happens during scope checking. Note that it is defined here
-- instead of in ./Error/Types to avoid orphan instances.
data ScopeError
  = ErrParser Text
  | ErrInfixParser InfixError
  | ErrInfixPattern InfixErrorP
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrLacksTypeSig LacksTypeSig
  | ErrImportCycle ImportCycle
  | ErrSymNotInScope NotInScope
  | ErrQualSymNotInScope QualifiedName
  | ErrModuleNotInScope ModuleNotInScope
  | ErrBindGroup BindGroupConflict
  | ErrDuplicateFixity DuplicateFixity
  | ErrMultipleExport MultipleExportConflict
  | ErrAmbiguousSym [SymbolEntry]
  | ErrAmbiguousModuleSym [SymbolEntry]
  -- | Eventually this needs to go away
  | ErrGeneric Text
  deriving stock (Show)

ppScopeError :: ScopeError -> Doc Eann
ppScopeError s = case s of
  ErrParser txt -> pretty txt
  ErrGeneric txt -> pretty txt
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
  ErrAmbiguousSym {} -> undefined
  ErrAmbiguousModuleSym {} -> undefined

docStream :: ScopeError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppScopeError

instance JuvixError ScopeError where
  renderAnsiText :: ScopeError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: ScopeError -> Text
  renderText = P.renderText . docStream
