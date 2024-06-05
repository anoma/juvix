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

data Inductive
  = IndBool
  | IndNat
  | IndInt
  | IndList
  | IndString
  | IndUser Name
  deriving stock (Eq)

data IndApp = IndApp
  { _indAppInductive :: Inductive,
    _indAppParams :: [Type]
  }
  deriving stock (Eq)

makeLenses ''Var
makeLenses ''FunType
makeLenses ''IndApp

data Statement
  = StmtDefinition Definition
  | StmtFunction Function
  | StmtSynonym Synonym
  | StmtDatatype Datatype
  | StmtRecord Record

data Definition = Definition
  { _definitionName :: Name,
    _definitionType :: Type
  }

data Function = Function
  { _functionName :: Name,
    _functionType :: Type
  }

data Synonym = Synonym
  { _synonymName :: Name,
    _synonymType :: Type
  }

data Datatype = Datatype
  { _datatypeName :: Name,
    _datatypeParams :: [Var],
    _datatypeConstructors :: [Constructor]
  }

data Constructor = Constructor
  { _constructorName :: Name,
    _constructorArgTypes :: [Type]
  }

data Record = Record
  { _recordName :: Name,
    _recordParams :: [Var],
    _recordFields :: [RecordField]
  }

data RecordField = RecordField
  { _recordFieldName :: Name,
    _recordFieldType :: Type
  }

makeLenses ''Definition
makeLenses ''Function
makeLenses ''Synonym
makeLenses ''Datatype
makeLenses ''Constructor
makeLenses ''Record
makeLenses ''RecordField

data Theory = Theory
  { _theoryName :: Name,
    _theoryImports :: [Name],
    _theoryStatements :: [Statement]
  }

makeLenses ''Theory

instance HasAtomicity Var where
  atomicity _ = Atom

instance HasAtomicity Type where
  atomicity = \case
    TyVar {} -> Atom
    TyFun {} -> Aggregate funFixity
    TyInd IndApp {..}
      | null _indAppParams -> Atom
      | otherwise -> Aggregate appFixity
