module Juvix.Compiler.Backend.Isabelle.Language
  ( module Juvix.Compiler.Backend.Isabelle.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Prelude,
  )
where

import Juvix.Compiler.Internal.Data.Name hiding (letFixity)
import Juvix.Prelude hiding (letFixity)

data Type
  = TyVar TypeVar
  | TyFun FunType
  | TyInd IndApp
  deriving stock (Eq)

data TypeVar = TypeVar
  { _typeVarName :: Name
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

makeLenses ''TypeVar
makeLenses ''FunType
makeLenses ''IndApp

data Expression
  = ExprIden Name
  | ExprUndefined
  | ExprLiteral Literal
  | ExprApp Application
  | ExprBinop Binop
  | ExprTuple (Tuple Expression)
  | ExprLet Let
  | ExprIf If
  | ExprCase Case
  | ExprLambda Lambda

data Literal
  = LitNumeric Integer
  | LitString Text

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression
  }

data Binop = Binop
  { _binopOperator :: Name,
    _binopLeft :: Expression,
    _binopRight :: Expression,
    _binopFixity :: Fixity
  }

data Let = Let
  { _letVar :: Name,
    _letValue :: Expression,
    _letBody :: Expression
  }

data If = If
  { _ifValue :: Expression,
    _ifBranchTrue :: Expression,
    _ifBranchFalse :: Expression
  }

data Case = Case
  { _caseValue :: Expression,
    _caseBranches :: NonEmpty CaseBranch
  }

data CaseBranch = CaseBranch
  { _caseBranchPattern :: Pattern,
    _caseBranchBody :: Expression
  }

data Lambda = Lambda
  { _lambdaVar :: Name,
    _lambdaType :: Maybe Type,
    _lambdaBody :: Expression
  }

data Pattern
  = PatVar Name
  | PatConstrApp ConstrApp
  | PatTuple (Tuple Pattern)

newtype Tuple a = Tuple
  { _tupleComponents :: NonEmpty a
  }

data ConstrApp = ConstrApp
  { _constrAppConstructor :: Name,
    _constrAppArgs :: [Pattern]
  }

makeLenses ''Application
makeLenses ''Let
makeLenses ''If
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''Lambda
makeLenses ''ConstrApp
makeLenses ''Expression

data Statement
  = StmtDefinition Definition
  | StmtFunction Function
  | StmtSynonym Synonym
  | StmtDatatype Datatype
  | StmtRecord Record

data Definition = Definition
  { _definitionName :: Name,
    _definitionType :: Type,
    _definitionBody :: Expression
  }

data Function = Function
  { _functionName :: Name,
    _functionType :: Type,
    _functionClauses :: NonEmpty Clause
  }

data Clause = Clause
  { _clausePatterns :: NonEmpty Pattern,
    _clauseBody :: Expression
  }

data Synonym = Synonym
  { _synonymName :: Name,
    _synonymType :: Type
  }

data Datatype = Datatype
  { _datatypeName :: Name,
    _datatypeParams :: [TypeVar],
    _datatypeConstructors :: [Constructor]
  }

data Constructor = Constructor
  { _constructorName :: Name,
    _constructorArgTypes :: [Type]
  }

data Record = Record
  { _recordName :: Name,
    _recordParams :: [TypeVar],
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
makeLenses ''Tuple

data Theory = Theory
  { _theoryName :: Name,
    _theoryImports :: [Name],
    _theoryStatements :: [Statement]
  }

makeLenses ''Theory

caseFixity :: Fixity
caseFixity =
  Fixity
    { _fixityPrecedence = PrecNat 0,
      _fixityArity = OpBinary AssocLeft,
      _fixityId = Nothing
    }

lambdaFixity :: Fixity
lambdaFixity =
  Fixity
    { _fixityPrecedence = PrecNat 0,
      _fixityArity = OpUnary AssocPostfix,
      _fixityId = Nothing
    }

ifFixity :: Fixity
ifFixity =
  Fixity
    { _fixityPrecedence = PrecNat 1,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

letFixity :: Fixity
letFixity =
  Fixity
    { _fixityPrecedence = PrecNat 2,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

instance HasAtomicity TypeVar where
  atomicity _ = Atom

instance HasAtomicity Type where
  atomicity = \case
    TyVar {} -> Atom
    TyFun {} -> Aggregate funFixity
    TyInd IndApp {..}
      | null _indAppParams -> Atom
      | otherwise -> Aggregate appFixity

instance HasAtomicity Expression where
  atomicity = \case
    ExprIden {} -> Atom
    ExprUndefined -> Atom
    ExprLiteral {} -> Atom
    ExprApp {} -> Aggregate appFixity
    ExprBinop Binop {..} -> Aggregate _binopFixity
    ExprTuple {} -> Atom
    ExprLet {} -> Aggregate letFixity
    ExprIf {} -> Aggregate ifFixity
    ExprCase {} -> Aggregate caseFixity
    ExprLambda {} -> Aggregate lambdaFixity
