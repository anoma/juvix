{-# LANGUAGE UndecidableInstances #-}

module MiniJuvix.Parsing.Language where

import qualified Data.Kind as GHC
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------
-- Parsing stages
--------------------------------------------------------------------------------

data Stage
  = Preparsed
  | Parsed
  deriving stock (Show)

type family ExpressionType (s :: Stage) :: GHC.Type where
  ExpressionType 'Preparsed = ExpressionSections
  ExpressionType 'Parsed = Expression

type family PatternType (s :: Stage) :: GHC.Type where
  PatternType 'Preparsed = PatternSection
  PatternType 'Parsed = Pattern

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

newtype Symbol = Sym Text
  deriving stock (Show)

-- A.B.C corresponds to ModulePath [A,B] C
data ModulePath
  = ModulePath
      { modulePathDir :: [Symbol],
        modulePathName :: Symbol
      }
  deriving stock (Show)

data Qualified
  = Qualified
      { modulePath :: ModulePath,
        nameSymbol :: Symbol
      }
  deriving stock (Show)

data Name
  = QualifiedName Qualified
  | Unqualified Symbol
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

data Statement (s :: Stage)
  = StatementOperator OperatorSyntaxDef
  | StatementTypeSignature (TypeSignature s)
  | StatementImport Import
  | StatementDataType (DataTypeDef s)
  | StatementModule (Module s)
  | StatementOpenModule OpenModule
  | StatementFunctionClause (FunctionClause s)
  | StatementAxiom (AxiomDef s)
  | StatementEval (Eval s)
  | StatementPrint (Print s)

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (Statement s)

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

newtype Import
  = Import
      { importModule :: ModulePath
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

type Precedence = Natural

data UnaryAssoc = AssocPrefix | AssocPostfix
  deriving stock (Show)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show)

data OperatorArity
  = Unary
      { unaryAssoc :: UnaryAssoc
      }
  | Binary
      { binaryAssoc :: BinaryAssoc
      }
  deriving stock (Show)

data OperatorSyntaxDef
  = OperatorSyntaxDef
      { opArity :: OperatorArity,
        opSymbol :: Symbol,
        opPrecedence :: Precedence
      }
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Usage
-------------------------------------------------------------------------------

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature (s :: Stage)
  = TypeSignature
      { sigName :: Symbol,
        sigType :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s :: Stage)
  = AxiomDef
      { axiomName :: Symbol,
        axiomType :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (AxiomDef s)

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

type DataConstructorName = Symbol

type DataTypeName = Symbol

data DataConstructorDef (s :: Stage)
  = DataConstructorDef
      { constructorName :: DataConstructorName,
        constructorType :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (DataConstructorDef s)

data DataTypeParameter (s :: Stage)
  = DataTypeParameter
      { dataTypeParameterName :: Symbol,
        dataTypeParameterType :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (DataTypeParameter s)

data DataTypeDef (s :: Stage)
  = DataTypeDef
      { dataTypeName :: DataTypeName,
        dataTypeParameters :: [DataTypeParameter s],
        dataTypeType :: Maybe (ExpressionType s),
        dataTypeConstructors :: [DataConstructorDef s]
      }

deriving stock instance Show (ExpressionType s) => Show (DataTypeDef s)

--------------------------------------------------------------------------------
-- Pattern
--------------------------------------------------------------------------------

data Pattern
  = PatternVariable Symbol
  | PatternConstructor DataConstructorName [Pattern]
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternSection
  = PatternSectionVariable Symbol
  | PatternSectionWildcard
  | PatternSectionEmpty
  | PatternSectionParen PatternSections
  deriving stock (Show)

newtype PatternSections = PatternSections (NonEmpty PatternSection)
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

data FunctionClause (s :: Stage)
  = FunClause
      { clauseOwnerFunction :: Symbol,
        clausePatterns :: [PatternType s],
        clauseBody :: ExpressionType s,
        clauseWhere :: Maybe (WhereBlock s)
      }

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (FunctionClause s)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data Module (s :: Stage)
  = Module
      { moduleName :: Symbol,
        moduleBody :: [Statement s]
      }

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (Module s)

data OpenModule
  = OpenModule
      { openModuleName :: Symbol,
        openUsing :: Maybe (NonEmpty Symbol),
        openHiding :: Maybe (NonEmpty Symbol)
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = ExprIdentifier Name
  | ExprApplication Application
  | ExprLambda (Lambda 'Parsed)
  | ExprLetBlock (LetBlock 'Parsed)
  | ExprUniverse Universe
  | ExprFunction (Function 'Parsed)
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Expression section
--------------------------------------------------------------------------------

-- | Expressions without application
data ExpressionSection
  = SectionIdentifier Name
  | SectionLambda (Lambda 'Preparsed)
  | SectionLetBlock (LetBlock 'Preparsed)
  | SectionUniverse Universe
  | SectionFunction (Function 'Preparsed)
  | SectionParens ExpressionSections
  deriving stock (Show)

newtype ExpressionSections = ExpressionSections (NonEmpty ExpressionSection)
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunctionParameter (s :: Stage)
  = FunctionParameter
      { paramName :: Maybe Symbol,
        paramUsage :: Maybe Usage,
        paramType :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (FunctionParameter s)

data Function (s :: Stage)
  = Function
      { funParameter :: FunctionParameter s,
        funReturn :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (Function s)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock (s :: Stage)
  = WhereBlock
      { whereClauses :: [WhereClause s]
      }

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (WhereBlock s)

data WhereClause (s :: Stage)
  = WhereOpenModule OpenModule
  | WhereTypeSig (TypeSignature s)
  | WhereFunClause (FunctionClause s)

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (WhereClause s)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

data Lambda (s :: Stage)
  = Lambda
      { lambdaParameters :: NonEmpty (PatternType s),
        lambdaBody :: ExpressionType s
      }

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (Lambda s)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application
  = Application
      { applicationFun :: ExpressionType 'Parsed,
        applicationArg :: ExpressionType 'Parsed
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

newtype LetBlock (s :: Stage) = LetBlock [LetClause s]

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (LetBlock s)

data LetClause (s :: Stage)
  = LetTypeSig (TypeSignature s)
  | LetDefinition (FunctionClause s)

deriving stock instance
  (Show (PatternType s), Show (ExpressionType s)) =>
  Show (LetClause s)

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

newtype Eval (s :: Stage)
  = Eval
      { evalExpression :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (Eval s)

newtype Print (s :: Stage)
  = Print
      { printExpression :: ExpressionType s
      }

deriving stock instance Show (ExpressionType s) => Show (Print s)
