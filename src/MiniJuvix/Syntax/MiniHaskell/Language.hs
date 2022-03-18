module MiniJuvix.Syntax.MiniHaskell.Language (
  module MiniJuvix.Syntax.MiniHaskell.Language,
  module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
  module MiniJuvix.Syntax.Concrete.Scoped.Name
   ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameId(..))
import MiniJuvix.Syntax.Fixity

type FunctionName = Name
type VarName = Name
type ConstrName = Name
type InductiveName = Name

data Name = Name {
    _nameText :: Text,
    _nameKind :: NameKind
    }
makeLenses ''Name

instance HasNameKind Name where
  getNameKind = _nameKind

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }

newtype ModuleBody = ModuleBody {
  _moduleStatements :: [Statement]
  }
  deriving newtype (Monoid, Semigroup)

data Statement =
  StatementInductiveDef InductiveDef
  | StatementFunctionDef FunctionDef

data FunctionDef = FunctionDef {
   _funDefName :: FunctionName,
   _funDefTypeSig :: Type,
   _funDefClauses :: NonEmpty FunctionClause
  }

data FunctionClause = FunctionClause {
    _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }

type Iden = Name

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  -- TODO Add a constructor for literals

data Application = Application {
  _appLeft :: Expression,
  _appRight :: Expression
  }

data Function = Function {
  _funLeft :: Type,
  _funRight :: Type
  }

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp {
  _constrAppConstructor :: Name,
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

type TypeIden = InductiveName

data Type =
  TypeIden TypeIden
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
