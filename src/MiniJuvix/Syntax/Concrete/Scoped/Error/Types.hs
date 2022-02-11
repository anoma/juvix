module MiniJuvix.Syntax.Concrete.Scoped.Error.Types (
  module MiniJuvix.Syntax.Concrete.Language,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Types
                                                    ) where

import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope

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

data MultipleDeclarations = MultipleDeclarations {
  _multipleDeclEntry :: SymbolEntry,
  _multipleDeclSymbol :: Text,
  _multipleDeclSecond :: Interval
  }
 deriving stock (Show)
