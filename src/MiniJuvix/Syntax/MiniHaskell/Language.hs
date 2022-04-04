module MiniJuvix.Syntax.MiniHaskell.Language
  ( module MiniJuvix.Syntax.MiniHaskell.Language,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.NameId,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.NameId
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import qualified MiniJuvix.Syntax.Concrete.Language as C
import MiniJuvix.Syntax.Fixity

type FunctionName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name

data Name = Name
  { _nameText :: Text,
    _nameKind :: NameKind
  }
  deriving stock (Show)

makeLenses ''Name

instance HasNameKind Name where
  getNameKind = _nameKind

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }
  deriving stock (Show)

newtype ModuleBody = ModuleBody
  { _moduleStatements :: [Statement]
  }
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)

data Statement
  = StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementVerbatim Text
  deriving stock (Show)

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefType :: Type,
    _funDefClauses :: NonEmpty FunctionClause
  }
  deriving stock (Show)

data FunctionClause = FunctionClause
  { _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }
  deriving stock (Show)

type Iden = Name

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionLiteral C.LiteralLoc
  | ExpressionVerbatim Text
  deriving stock (Show)

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression
  }
  deriving stock (Show)

data Function = Function
  { _funLeft :: Type,
    _funRight :: Type
  }
  deriving stock (Show)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp
  { _constrAppConstructor :: Name,
    _constrAppParameters :: [Pattern]
  }
  deriving stock (Show)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard
  deriving stock (Show)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveConstructors :: [InductiveConstructorDef]
  }
  deriving stock (Show)

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorParameters :: [Type]
  }
  deriving stock (Show)

data TypeIden
  = TypeIdenInductive InductiveName
  deriving stock (Show)

data Type
  = TypeIden TypeIden
  | TypeFunction Function
  | TypeVerbatim Text
  deriving stock (Show)

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''ModuleBody
makeLenses ''Application
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
    ExpressionApplication a -> atomicity a
    ExpressionVerbatim {} -> Atom
    ExpressionLiteral l -> atomicity l

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Type where
  atomicity t = case t of
    TypeIden {} -> Atom
    TypeFunction f -> atomicity f
    TypeVerbatim {} -> Atom

instance HasAtomicity ConstructorApp where
  atomicity (ConstructorApp _ args)
    | null args = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Pattern where
  atomicity p = case p of
    PatternConstructorApp a -> atomicity a
    PatternVariable {} -> Atom
    PatternWildcard {} -> Atom
