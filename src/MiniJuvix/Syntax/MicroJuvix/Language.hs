module MiniJuvix.Syntax.MicroJuvix.Language
  ( module MiniJuvix.Syntax.MicroJuvix.Language,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.Concrete.Scoped.Name,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language (ForeignBlock (..))
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameId (..))
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Fixity

type FunctionName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind
  }

makeLenses ''Name

instance Eq Name where
  (==) = (==) `on` _nameId

instance Ord Name where
  compare = compare `on` _nameId

instance Hashable Name where
  hashWithSalt salt = hashWithSalt salt . _nameId

instance HasNameKind Name where
  getNameKind = _nameKind

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }

data ModuleBody = ModuleBody
  { _moduleStatements :: [Statement]
  }

data Statement =
  StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementForeign ForeignBlock

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefTypeSig :: Type,
    _funDefClauses :: NonEmpty FunctionClause
  }

data FunctionClause = FunctionClause
  { _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }

data Iden
  = IdenFunction Name
  | IdenConstructor Name
  | IdenVar VarName

data TypedExpression = TypedExpression {
  _typedType :: Type,
  _typedExpression :: Expression
  }

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionTyped TypedExpression

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression
  }

data Function = Function
  { _funLeft :: Type,
    _funRight :: Type
  }
  deriving stock (Eq)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp
  { _constrAppConstructor :: Name,
    _constrAppParameters :: [Pattern]
  }

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveConstructors :: [InductiveConstructorDef]
  }

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorParameters :: [Type]
  }

newtype TypeIden
  = TypeIdenInductive InductiveName
  deriving stock (Eq)

data Type
  = TypeIden TypeIden
  | TypeFunction Function
  deriving stock (Eq)

data ConstructorInfo = ConstructorInfo {
  _constructorInfoArgs :: [Type],
  _constructorInfoInductive :: InductiveName
  }

data FunctionInfo = FunctionInfo {
  _functionInfoType :: Type
  }

data InfoTable = InfoTable {
  _infoConstructors :: HashMap Name ConstructorInfo,
  _infoFunctions :: HashMap Name FunctionInfo
  }

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''FunctionClause
makeLenses ''InductiveDef
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

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

instance HasAtomicity Type where
  atomicity t = case t of
    TypeIden {} -> Atom
    TypeFunction f -> atomicity f

instance HasAtomicity ConstructorApp where
  atomicity (ConstructorApp _ args)
    | null args = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Pattern where
  atomicity p = case p of
    PatternConstructorApp a -> atomicity a
    PatternVariable {} -> Atom
    PatternWildcard {} -> Atom
