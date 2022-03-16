module MiniJuvix.Syntax.Concrete.Scoped.Error.Types (
  module MiniJuvix.Syntax.Concrete.Language,
  module MiniJuvix.Syntax.Concrete.Scoped.Error.Types
                                                    ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

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

-- | type signature without a function clause
newtype LacksFunctionClause = LacksFunctionClause {
  _lacksFunctionClause :: TypeSignature 'Scoped
  }
 deriving stock (Show)

newtype ImportCycle = ImportCycle {
  -- | If we have [a, b, c] it means that a import b imports c imports a.
  _importCycleImports :: NonEmpty (Import 'Parsed)
  }
 deriving stock (Show)

data BindGroupConflict = BindGroupConflict {
 _bindGroupFirst :: Symbol,
 _bindGroupSecond :: Symbol
 }
 deriving stock (Show)

data DuplicateFixity = DuplicateFixity {
 _dupFixityFirst :: OperatorSyntaxDef,
 _dupFixitySecond :: OperatorSyntaxDef
 }
 deriving stock (Show)

data MultipleExportConflict = MultipleExportConflict {
  _multipleExportModule :: S.AbsModulePath,
  _multipleExportSymbol :: Symbol,
  _multipleExportEntries :: NonEmpty SymbolEntry
  }
 deriving stock (Show)

data NotInScope = NotInScope {
 _notInScopeSymbol :: Symbol,
 _notInScopeLocal :: LocalVars,
 _notInScopeScope :: Scope
 }
 deriving stock (Show)

newtype ModuleNotInScope = ModuleNotInScope {
  _moduleNotInScopeName :: Name
  }
 deriving stock (Show)

newtype MegaParsecError = MegaParsecError {
  _megaParsecError :: Text
  }
 deriving stock (Show)

newtype UnusedOperatorDef = UnusedOperatorDef {
  _unusedOperatorDef :: OperatorSyntaxDef
  }
 deriving stock (Show)

data WrongTopModuleName = WrongTopModuleName {
  _wrongTopModuleNameExpectedPath :: FilePath,
  _wrongTopModuleNameActualPath :: FilePath,
  _wrongTopModuleNameActualName :: TopModulePath
  }
 deriving stock (Show)

newtype AmbiguousSym = AmbiguousSym {
  _ambiguousSymEntires :: [SymbolEntry]
  }
 deriving stock (Show)

newtype AmbiguousModuleSym = AmbiguousModuleSym {
  _ambiguousModSymEntires :: [SymbolEntry]
  }
 deriving stock (Show)
