module Juvix.Compiler.Backend.Isabelle.Language
  ( module Juvix.Compiler.Backend.Isabelle.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Prelude,
  )
where

import Juvix.Compiler.Internal.Data.Name
import Juvix.Prelude

data Type
  = TyVar Var
  | TyBool
  | TyNat
  | TyInt
  | TyFun FunType
  | TyInd IndApp
  deriving stock (Eq)

data Var = Var
  { _varName :: Name
  }
  deriving stock (Eq)

data FunType = FunType
  { _funTypeLeft :: Type,
    _funTypeRight :: Type
  }
  deriving stock (Eq)

data IndApp = IndApp
  { _indAppInductive :: Name,
    _indAppParams :: [Type]
  }
  deriving stock (Eq)

makeLenses ''Var
makeLenses ''FunType
makeLenses ''IndApp

data Statement
  = StmtDefinition Definition
  | StmtFunction Function
  | StmtDatatype Datatype
  | StmtSynonym Synonym

data Definition = Definition
  { _definitionName :: Name,
    _definitionType :: Type
  }

data Function = Function
  { _functionName :: Name,
    _functionType :: Type
  }

data Datatype = Datatype
  { _datatypeName :: Name,
    _datatypeParams :: [Var],
    _datatypeConstructors :: [Constructor]
  }

data Constructor = Constructor
  { _constructorName :: Name,
    _constructorArgTypes :: [Type],
    _constructorFixity :: Maybe Fixity
  }

data Synonym = Synonym
  { _synonymName :: Name,
    _synonymType :: Type
  }

makeLenses ''Definition
makeLenses ''Function
makeLenses ''Datatype
makeLenses ''Constructor
makeLenses ''Synonym

data Theory = Theory
  { _theoryName :: Name,
    _theoryImports :: [Name],
    _theoryStatements :: [Statement]
  }

makeLenses ''Theory

instance HasAtomicity Type where
  atomicity = \case
    TyVar {} -> Atom
    TyBool -> Atom
    TyNat -> Atom
    TyInt -> Atom
    TyFun {} -> Aggregate funFixity
    TyInd IndApp {..}
      | null _indAppParams -> Atom
      | otherwise -> Aggregate appFixity
