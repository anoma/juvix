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
  { _moduleInductives :: HashMap InductiveName (Indexed InductiveDef),
    _moduleFunctions :: HashMap FunctionName (Indexed FunctionDef),
    _moduleForeigns :: [Indexed ForeignBlock]
  }

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

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression
  }

data Function = Function
  { _funLeft :: Type,
    _funRight :: Type
  }

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

data Type
  = TypeIden TypeIden
  | TypeFunction Function

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''ModuleBody
makeLenses ''Application
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp

instance Semigroup ModuleBody where
  a <> b =
    ModuleBody
      { _moduleInductives = a ^. moduleInductives <> b ^. moduleInductives,
        _moduleFunctions = a ^. moduleFunctions <> b ^. moduleFunctions,
        _moduleForeigns = a ^. moduleForeigns <> b ^. moduleForeigns
      }

instance Monoid ModuleBody where
  mempty =
    ModuleBody
      { _moduleInductives = mempty,
        _moduleForeigns = mempty,
        _moduleFunctions = mempty
      }

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
    ExpressionApplication a -> atomicity a

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
