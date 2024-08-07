module Juvix.Compiler.Backend.Isabelle.Language
  ( module Juvix.Compiler.Backend.Isabelle.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Prelude,
  )
where

import Juvix.Compiler.Internal.Data.Name hiding (letFixity)
import Juvix.Prelude hiding (Cons, letFixity)

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
  | IndOption
  | IndTuple
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
  | ExprList (List Expression)
  | ExprCons (Cons Expression)
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
  { _letClauses :: NonEmpty LetClause,
    _letBody :: Expression
  }

data LetClause = LetClause
  { _letClauseName :: Name,
    _letClauseValue :: Expression
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
  | PatZero
  | PatConstrApp ConstrApp
  | PatTuple (Tuple Pattern)
  | PatList (List Pattern)
  | PatCons (Cons Pattern)

newtype Tuple a = Tuple
  { _tupleComponents :: NonEmpty a
  }

newtype List a = List
  { _listElements :: [a]
  }

data Cons a = Cons
  { _consHead :: a,
    _consTail :: a
  }

data ConstrApp = ConstrApp
  { _constrAppConstructor :: Name,
    _constrAppArgs :: [Pattern]
  }

makeLenses ''Application
makeLenses ''Let
makeLenses ''LetClause
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

consFixity :: Fixity
consFixity =
  Fixity
    { _fixityPrecedence = PrecNat 80,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

andFixity :: Fixity
andFixity =
  Fixity
    { _fixityPrecedence = PrecNat 35,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

orFixity :: Fixity
orFixity =
  Fixity
    { _fixityPrecedence = PrecNat 30,
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
    ExprList {} -> Atom
    ExprCons {} -> Aggregate consFixity
    ExprLet {} -> Aggregate letFixity
    ExprIf {} -> Aggregate ifFixity
    ExprCase {} -> Aggregate caseFixity
    ExprLambda {} -> Aggregate lambdaFixity

instance HasAtomicity Pattern where
  atomicity = \case
    PatVar {} -> Atom
    PatZero -> Atom
    PatConstrApp ConstrApp {..}
      | null _constrAppArgs -> Atom
      | otherwise -> Aggregate appFixity
    PatTuple {} -> Atom
    PatList {} -> Atom
    PatCons {} -> Aggregate consFixity
