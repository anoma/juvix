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
  | ExprRecord (Record Expression)
  | ExprRecordUpdate RecordUpdate
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

data RecordUpdate = RecordUpdate
  { _recordUpdateRecord :: Expression,
    _recordUpdateFields :: Record Expression
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
  | PatRecord (Record Pattern)

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

data Record a = Record
  { _recordName :: Name,
    _recordFields :: [(Name, a)]
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
makeLenses ''Binop
makeLenses ''RecordUpdate
makeLenses ''Tuple
makeLenses ''List
makeLenses ''Cons
makeLenses ''Record

data Statement
  = StmtDefinition Definition
  | StmtFunction Function
  | StmtSynonym Synonym
  | StmtDatatype Datatype
  | StmtRecord RecordDef

data Definition = Definition
  { _definitionName :: Name,
    _definitionType :: Type,
    _definitionBody :: Expression,
    _definitionDocComment :: Maybe Text
  }

data Function = Function
  { _functionName :: Name,
    _functionType :: Type,
    _functionClauses :: NonEmpty Clause,
    _functionDocComment :: Maybe Text
  }

data Clause = Clause
  { _clausePatterns :: NonEmpty Pattern,
    _clauseBody :: Expression
  }

data Synonym = Synonym
  { _synonymName :: Name,
    _synonymType :: Type,
    _synonymDocComment :: Maybe Text
  }

data Datatype = Datatype
  { _datatypeName :: Name,
    _datatypeParams :: [TypeVar],
    _datatypeConstructors :: [Constructor],
    _datatypeDocComment :: Maybe Text
  }

data Constructor = Constructor
  { _constructorName :: Name,
    _constructorArgTypes :: [Type],
    _constructorDocComment :: Maybe Text
  }

data RecordDef = RecordDef
  { _recordDefName :: Name,
    _recordDefParams :: [TypeVar],
    _recordDefFields :: [RecordField],
    _recordDefDocComment :: Maybe Text
  }

data RecordField = RecordField
  { _recordFieldName :: Name,
    _recordFieldType :: Type,
    _recordFieldDocComment :: Maybe Text
  }

makeLenses ''Definition
makeLenses ''Function
makeLenses ''Synonym
makeLenses ''Datatype
makeLenses ''Constructor
makeLenses ''RecordField

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
    ExprRecord {} -> Aggregate appFixity
    ExprRecordUpdate {} -> Aggregate appFixity
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
    PatRecord {} -> Atom
