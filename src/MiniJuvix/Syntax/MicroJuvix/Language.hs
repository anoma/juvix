module MiniJuvix.Syntax.MicroJuvix.Language
  ( module MiniJuvix.Syntax.MicroJuvix.Language,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.Concrete.Scoped.Name,
    module MiniJuvix.Syntax.Concrete.Loc,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language (LiteralLoc)
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameId (..))
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.ForeignBlock
import Prettyprinter

type FunctionName = Name

type ConstructorName = Name

type AxiomName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind,
    _nameDefined :: Interval,
    _nameLoc :: Interval
  }
  deriving stock (Show)

instance HasLoc Name where
  getLoc = _nameLoc

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
  pretty n =
    pretty (n ^. nameText)
      <> "@"
      <> pretty (n ^. nameId)

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
    _axiomType :: Type
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
  | IdenInductive Name
  deriving stock (Show)

data TypedExpression = TypedExpression
  { _typedType :: Type,
    _typedExpression :: Expression
  }
  deriving stock (Show)

data FunctionExpression = FunctionExpression
  { _functionExpressionLeft :: Expression,
    _functionExpressionRight :: Expression
  }
  deriving stock (Show)

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionFunction FunctionExpression
  | ExpressionLiteral LiteralLoc
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
  deriving stock (Show, Generic, Eq)

instance Hashable Function

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

newtype InductiveParameter = InductiveParameter
  { _inductiveParamName :: VarName
  }
  deriving stock (Show, Eq)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveParameters :: [InductiveParameter],
    _inductiveConstructors :: [InductiveConstructorDef]
  }

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorParameters :: [Type]
  }

data TypeIden
  = TypeIdenInductive InductiveName
  | TypeIdenAxiom AxiomName
  | TypeIdenVariable VarName
  deriving stock (Show, Eq, Generic)

instance Hashable TypeIden

data TypeApplication = TypeApplication
  { _typeAppLeft :: Type,
    _typeAppRight :: Type
  }
  deriving stock (Show, Generic, Eq)

instance Hashable TypeApplication

data TypeAbstraction = TypeAbstraction
  { _typeAbsVar :: VarName,
    _typeAbsBody :: Type
  }
  deriving stock (Show, Eq, Generic)

instance Hashable TypeAbstraction

data Type
  = TypeIden TypeIden
  | TypeApp TypeApplication
  | TypeFunction Function
  | TypeAbs TypeAbstraction
  | TypeUniverse
  | TypeAny
  deriving stock (Eq, Show, Generic)

instance Hashable Type

data FunctionArgType
  = FunctionArgTypeAbstraction VarName
  | FunctionArgTypeType Type
  deriving stock (Show)

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionExpression
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''AxiomDef
makeLenses ''ModuleBody
makeLenses ''Application
makeLenses ''TypedExpression
makeLenses ''TypeAbstraction
makeLenses ''TypeApplication
makeLenses ''InductiveParameter
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp

instance HasAtomicity Name where
  atomicity = const Atom

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity TypeApplication where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity FunctionExpression where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
    ExpressionApplication a -> atomicity a
    ExpressionTyped t -> atomicity (t ^. typedExpression)
    ExpressionLiteral l -> atomicity l
    ExpressionFunction f -> atomicity f

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity TypeAbstraction where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Type where
  atomicity t = case t of
    TypeIden {} -> Atom
    TypeFunction f -> atomicity f
    TypeUniverse -> Atom
    TypeAny -> Atom
    TypeAbs a -> atomicity a
    TypeApp a -> atomicity a

instance HasAtomicity ConstructorApp where
  atomicity (ConstructorApp _ args)
    | null args = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Pattern where
  atomicity p = case p of
    PatternConstructorApp a -> atomicity a
    PatternVariable {} -> Atom
    PatternWildcard {} -> Atom

instance HasLoc FunctionExpression where
  getLoc (FunctionExpression l r) = getLoc l <> getLoc r

instance HasLoc Expression where
  getLoc = \case
    ExpressionIden i -> getLoc i
    ExpressionApplication a -> getLoc (a ^. appLeft)
    ExpressionTyped t -> getLoc (t ^. typedExpression)
    ExpressionLiteral l -> getLoc l
    ExpressionFunction f -> getLoc f

instance HasLoc Iden where
  getLoc = \case
    IdenFunction f -> getLoc f
    IdenConstructor c -> getLoc c
    IdenVar v -> getLoc v
    IdenAxiom a -> getLoc a
    IdenInductive a -> getLoc a
