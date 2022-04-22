module MiniJuvix.Syntax.MonoJuvix.Language
  ( module MiniJuvix.Syntax.MonoJuvix.Language,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.Concrete.Scoped.Name,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Concrete.Language qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameId (..))
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.ForeignBlock
import Prettyprinter

type FunctionName = Name

type AxiomName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind,
    _nameDefined :: Maybe C.Interval,
    _nameLoc :: Maybe C.Interval
  }
  deriving stock (Show)

makeLenses ''Name

instance Eq Name where
  (==) = (==) `on` _nameId

instance Ord Name where
  compare = compare `on` _nameId

instance Hashable Name where
  hashWithSalt salt = hashWithSalt salt . _nameId

instance HasNameKind Name where
  getNameKind = _nameKind

instance Pretty Name where
  pretty = pretty . _nameText

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }

newtype ModuleBody = ModuleBody
  { _moduleStatements :: [Statement]
  }

data Statement
  = StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementForeign ForeignBlock
  | StatementAxiom AxiomDef

data AxiomDef = AxiomDef
  { _axiomName :: AxiomName,
    _axiomType :: Type,
    _axiomBackendItems :: [BackendItem]
  }

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefType :: Type,
    _funDefClauses :: NonEmpty FunctionClause
  }

data FunctionClause = FunctionClause
  { _clauseName :: FunctionName,
    _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }
  deriving stock (Show)

data Iden
  = IdenFunction Name
  | IdenConstructor Name
  | IdenVar VarName
  | IdenAxiom Name
  deriving stock (Show)

data TypedExpression = TypedExpression
  { _typedType :: Type,
    _typedExpression :: Expression
  }
  deriving stock (Show)

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionLiteral C.LiteralLoc
  | ExpressionTyped TypedExpression
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
  deriving stock (Show, Eq)

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

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorParameters :: [Type]
  }

data TypeIden
  = TypeIdenInductive InductiveName
  | TypeIdenAxiom AxiomName
  deriving stock (Show, Eq)

data Type
  = TypeIden TypeIden
  | TypeFunction Function
  | TypeUniverse
  | TypeAny
  deriving stock (Show, Eq)

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''AxiomDef
makeLenses ''ModuleBody
makeLenses ''Application
makeLenses ''TypedExpression
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
    ExpressionApplication a -> atomicity a
    ExpressionTyped t -> atomicity (t ^. typedExpression)
    ExpressionLiteral l -> atomicity l

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Type where
  atomicity t = case t of
    TypeIden {} -> Atom
    TypeFunction f -> atomicity f
    TypeUniverse -> Atom
    TypeAny -> Atom

instance HasAtomicity ConstructorApp where
  atomicity (ConstructorApp _ args)
    | null args = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Pattern where
  atomicity p = case p of
    PatternConstructorApp a -> atomicity a
    PatternVariable {} -> Atom
    PatternWildcard {} -> Atom
