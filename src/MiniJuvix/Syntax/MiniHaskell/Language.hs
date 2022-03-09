module MiniJuvix.Syntax.MiniHaskell.Language (
  module MiniJuvix.Syntax.MiniHaskell.Language,
  module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
  module MiniJuvix.Syntax.Concrete.Scoped.Name
   ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameId(..))

type FunctionName = Name
type VarName = Name
type ConstrName = Name
type InductiveName = Name

data Name = Name {
    _nameText :: Text,
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

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }

data ModuleBody = ModuleBody {
  _moduleInductives :: HashMap InductiveName InductiveDef,
  _moduleFunctions :: HashMap FunctionName FunctionDef
  }

data FunctionDef = FunctionDef {
   _funDefName :: FunctionName,
   _funDefTypeSig :: Type,
   _funDefClauses :: NonEmpty FunctionClause
  }

data FunctionClause = FunctionClause {
    _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }

data Iden =
  IdenDefined Name
  | IdenConstructor Name
  | IdenVar VarName
  | IdenAxiom Name

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionFunction Function

data Application = Application {
  _appLeft :: Expression,
  _appRight :: Expression
  }

data Function = Function
  { _funParameter :: Type,
    _funReturn :: Type
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
    _inductiveType :: Maybe Expression,
    _inductiveConstructors :: [InductiveConstructorDef]
  }

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorType :: Type
  }

data Type =
  TypeInductive InductiveName
 | TypeFunction Function

makeLenses ''Module
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''ModuleBody
makeLenses ''InductiveConstructorDef
