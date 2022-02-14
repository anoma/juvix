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

-- | megaparsec error while resolving infixities.
newtype InfixError = InfixError {
  _infixErrAtoms :: ExpressionAtoms 'Scoped
  }
 deriving stock (Show)

-- | megaparsec error while resolving infixities of patterns.
newtype InfixErrorP = InfixErrorP {
  _infixErrAtomsP :: PatternAtom 'Scoped
  }
 deriving stock (Show)

-- | function clause without a type signature.
newtype LacksTypeSig = LacksTypeSig {
  _lacksTypeSigClause :: FunctionClause 'Parsed
  }
 deriving stock (Show)

newtype ImportCycle = ImportCycle {
  _importCycleImport :: Import 'Parsed
  }
 deriving stock (Show)

data NotInScope = NotInScope {
 _notInScopeSymbol :: Symbol,
 _notInScopeLocal :: LocalVars,
 _notInScopeScope :: Scope
 }
 deriving stock (Show)
