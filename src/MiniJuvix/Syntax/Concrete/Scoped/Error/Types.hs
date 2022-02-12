module MiniJuvix.Syntax.Concrete.Scoped.Error.Types (
  module MiniJuvix.Syntax.Concrete.Language,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Types
                                                    ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope

data MultipleDeclarations = MultipleDeclarations {
  _multipleDeclEntry :: SymbolEntry,
  _multipleDeclSymbol :: Text,
  _multipleDeclSecond :: Interval
  }
 deriving stock (Show)
