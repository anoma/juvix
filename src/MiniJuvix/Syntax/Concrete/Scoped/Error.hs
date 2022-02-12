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
  | ErrInfixParser Text
  | ErrInfixPattern Text
  | ErrMultipleDeclarations MultipleDeclarations
  | ErrLacksTypeSig Symbol
  | ErrImportCycle TopModulePath
  | ErrOpenNotInScope QualifiedName
  | ErrSymNotInScope Symbol Scope LocalVars
  | ErrQualSymNotInScope QualifiedName
  | ErrModuleNotInScope Name
  | ErrBindGroup Symbol
  | ErrDuplicateFixity Symbol
  | ErrMultipleExport Symbol
  | ErrAmbiguousSym [SymbolEntry]
  | ErrAmbiguousModuleSym [SymbolEntry]
  -- | Eventually this needs to go away
  | ErrGeneric Text
  deriving stock (Show)

ppScopeError :: ScopeError -> Doc Eann
ppScopeError e = case e of
  ErrParser txt -> pretty txt
  ErrGeneric txt -> pretty txt
  ErrInfixParser txt -> pretty txt
  ErrInfixPattern txt -> pretty txt
  ErrMultipleDeclarations er -> ppMultipleDeclarations er
  ErrLacksTypeSig {} -> ugly
  ErrImportCycle {} -> ugly
  ErrOpenNotInScope {} -> ugly
  ErrSymNotInScope {} -> ugly
  ErrQualSymNotInScope {} -> ugly
  ErrModuleNotInScope {} -> ugly
  ErrBindGroup {} -> ugly
  ErrDuplicateFixity {} -> ugly
  ErrMultipleExport {} -> ugly
  ErrAmbiguousSym {} -> ugly
  ErrAmbiguousModuleSym {} -> ugly
  where
  ugly = pretty (show e :: Text)

docStream :: ScopeError -> SimpleDocStream Eann
docStream = layoutPretty defaultLayoutOptions . ppScopeError

instance JuvixError ScopeError where
  renderAnsiText :: ScopeError -> Text
  renderAnsiText = renderAnsi . docStream

  renderText :: ScopeError -> Text
  renderText = P.renderText . docStream
