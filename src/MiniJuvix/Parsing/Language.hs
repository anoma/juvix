{-# language DeriveGeneric #-}
{-# language UndecidableInstances #-}
-- | Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix
module MiniJuvix.Parsing.Language where


import MiniJuvix.Utils.Prelude
import qualified Data.Kind                   as GHC
import Data.Singletons

--------------------------------------------------------------------------------
-- Parsing stage
--------------------------------------------------------------------------------

data Stage =
  Parsed
  | Scoped
  deriving stock (Show)

type family ExpressionType (s ∷ Stage) ∷ GHC.Type where
  ExpressionType 'Parsed = (ExpressionSections 'Parsed)
  ExpressionType 'Scoped = Expression

type family PatternType (s ∷ Stage) ∷ GHC.Type where
  PatternType 'Parsed = (PatternSection 'Parsed)
  PatternType 'Scoped = Pattern

type family ImportType (s ∷ Stage) ∷ GHC.Type where
  ImportType 'Parsed = Import
  ImportType 'Scoped = Module 'Scoped 'ModuleTop

type family ModulePathType (t ∷ ModuleIsTop) ∷ (res ∷ GHC.Type) | res → t where
  ModulePathType 'ModuleTop = ModulePath
  ModulePathType 'ModuleLocal = Symbol

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

newtype Symbol = Sym Text
  deriving stock (Show, Eq, Ord)

instance Hashable Symbol where
  hashWithSalt i (Sym t) = hashWithSalt i t

-- A.B.C corresponds to ModulePath [A,B] C
data ModulePath = ModulePath {
  modulePathDir ∷ [Symbol],
  modulePathName ∷ Symbol
  }
  deriving stock (Show, Eq, Ord, Generic)
instance Hashable ModulePath

data QualifiedName = QualifiedName {
  qualifiedModulePath ∷ ModulePath,
  qualifiedNameSymbol ∷ Symbol
  }
  deriving stock (Show, Eq, Ord)

data Name =
  NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

data Statement (s ∷ Stage)
  = StatementOperator OperatorSyntaxDef
  | StatementTypeSignature (TypeSignature s)
  | StatementImport (ImportType s)
  | StatementDataType (DataTypeDef s)
  | StatementModule (Module s 'ModuleLocal)
  | StatementOpenModule OpenModule
  | StatementFunctionClause (FunctionClause s)
  | StatementAxiom (AxiomDef s)
  | StatementEval (Eval s)
  | StatementPrint (Print s)
deriving stock instance (Show (ImportType s), Show (PatternType s), Show (ExpressionType s)) ⇒ Show (Statement s)
deriving stock instance (Eq (ImportType s), Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (Statement s)
deriving stock instance (Ord (ImportType s), Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (Statement s)

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

newtype Import = Import {
  importModule ∷ ModulePath
  }
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

type Precedence = Natural

data UnaryAssoc = AssocPrefix | AssocPostfix
  deriving stock (Show, Eq, Ord)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show, Eq, Ord)

data OperatorArity =
  Unary {
   unaryAssoc ∷ UnaryAssoc
  }
  | Binary {
    binaryAssoc ∷ BinaryAssoc
  }
  deriving stock (Show, Eq, Ord)

data OperatorSyntaxDef =
  OperatorSyntaxDef {
  opArity ∷ OperatorArity
  , opSymbol ∷ Symbol
  , opPrecedence ∷ Precedence
  }
  deriving stock (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Usage
-------------------------------------------------------------------------------

data Usage =
  UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature (s ∷ Stage)
  = TypeSignature
      {
        sigName ∷ Symbol,
        sigType ∷ ExpressionType s
      }
deriving stock instance Show (ExpressionType s) ⇒ Show (TypeSignature s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (TypeSignature s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s ∷ Stage) = AxiomDef {
   axiomName ∷ Symbol,
   axiomType ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (AxiomDef s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (AxiomDef s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (AxiomDef s)

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

type DataConstructorName = Symbol

type DataTypeName = Symbol

data DataConstructorDef (s ∷ Stage) = DataConstructorDef {
  constructorName ∷ DataConstructorName
  , constructorType ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (DataConstructorDef s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (DataConstructorDef s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (DataConstructorDef s)


data DataTypeParameter (s ∷ Stage) = DataTypeParameter {
  dataTypeParameterName ∷ Symbol,
  dataTypeParameterType ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (DataTypeParameter s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (DataTypeParameter s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (DataTypeParameter s)

data DataTypeDef (s ∷ Stage)
  = DataTypeDef
      { dataTypeName ∷ DataTypeName,
        dataTypeParameters ∷ [DataTypeParameter s],
        dataTypeType ∷ Maybe (ExpressionType s),
        dataTypeConstructors ∷ [DataConstructorDef s]
      }
deriving stock instance Show (ExpressionType s) ⇒ Show (DataTypeDef s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (DataTypeDef s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (DataTypeDef s)


--------------------------------------------------------------------------------
-- Pattern
--------------------------------------------------------------------------------

data Pattern
  = PatternVariable Symbol
  | PatternAppConstructor QualifiedName Pattern
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternSection (s ∷ Stage)
  = PatternSectionVariable Symbol
  | PatternSectionWildcard
  | PatternSectionEmpty
  | PatternSectionParen (PatternSections s)
deriving stock instance (Show (ExpressionType s), Show (PatternType s)) ⇒ Show (PatternSection s)
deriving stock instance (Eq (ExpressionType s), Eq (PatternType s)) ⇒ Eq (PatternSection s)
deriving stock instance (Ord (ExpressionType s), Ord (PatternType s)) ⇒ Ord (PatternSection s)

newtype PatternSections (s ∷ Stage) = PatternSections (NonEmpty (PatternSection s))
deriving stock instance (Show (ExpressionType s), Show (PatternType s)) ⇒ Show (PatternSections s)
deriving stock instance (Eq (ExpressionType s), Eq (PatternType s)) ⇒ Eq (PatternSections s)
deriving stock instance (Ord (ExpressionType s), Ord (PatternType s)) ⇒ Ord (PatternSections s)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type FunctionName = Symbol

data FunctionClause (s ∷ Stage)
  = FunClause
      { clauseOwnerFunction ∷ FunctionName,
        clausePatterns ∷ [PatternType s],
        clauseBody ∷ ExpressionType s,
        clauseWhere ∷ Maybe (WhereBlock s)
      }
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (FunctionClause s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (FunctionClause s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (FunctionClause s)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data ModuleIsTop = ModuleTop | ModuleLocal

-- The following Singleton related definitions could be scrapped if we depended
-- on the singletons-th library.
data SModuleIsTop (t ∷ ModuleIsTop) where
  SModuleTop ∷ SModuleIsTop 'ModuleTop
  SModuleLocal ∷ SModuleIsTop 'ModuleLocal

type instance Sing = SModuleIsTop

instance SingI 'ModuleTop where
  sing = SModuleTop

instance SingI 'ModuleLocal where
  sing = SModuleLocal

data Module (s ∷ Stage) (t ∷ ModuleIsTop)
  = Module
      { moduleModulePath ∷ ModulePathType t,
        moduleBody ∷ [Statement s]
      }
deriving stock instance (Show (ModulePathType t), Show (ImportType s), Show (PatternType s), Show (ExpressionType s)) ⇒ Show (Module s t)
deriving stock instance (Eq (ModulePathType t), Eq (ImportType s), Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (Module s t)
deriving stock instance (Ord (ModulePathType t), Ord (ImportType s), Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (Module s t)

data OpenModule = OpenModule {
  openModuleName ∷ Symbol,
  openUsing ∷ Maybe (NonEmpty Symbol),
  openHiding ∷ Maybe (NonEmpty Symbol)
  }
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = ExprIdentifier QualifiedName
  | ExprApplication Application
  | ExprLambda (Lambda 'Scoped)
  | ExprLetBlock (LetBlock 'Scoped)
  | ExprUniverse Universe
  | ExprFunction (Function 'Scoped)
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Expression section
--------------------------------------------------------------------------------

-- | Expressions without application
data ExpressionSection (s ∷ Stage)
  = SectionIdentifier Name
  | SectionLambda (Lambda s)
  | SectionLetBlock (LetBlock s)
  | SectionUniverse Universe
  | SectionFunction (Function s)
  | SectionParens (ExpressionSections s)
deriving stock instance (Show (ExpressionType s), Show (PatternType s)) ⇒ Show (ExpressionSection s)
deriving stock instance (Eq (ExpressionType s), Eq (PatternType s)) ⇒ Eq (ExpressionSection s)
deriving stock instance (Ord (ExpressionType s), Ord (PatternType s)) ⇒ Ord (ExpressionSection s)

newtype ExpressionSections (s ∷ Stage) = ExpressionSections (NonEmpty (ExpressionSection s))
deriving stock instance (Show (ExpressionType s), Show (PatternType s)) ⇒ Show (ExpressionSections s)
deriving stock instance (Eq (ExpressionType s), Eq (PatternType s)) ⇒ Eq (ExpressionSections s)
deriving stock instance (Ord (ExpressionType s), Ord (PatternType s)) ⇒ Ord (ExpressionSections s)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunctionParameter (s ∷ Stage) = FunctionParameter {
  paramName ∷ Maybe Symbol,
  paramUsage ∷ Maybe Usage,
  paramType ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (FunctionParameter s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (FunctionParameter s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (FunctionParameter s)


data Function (s ∷ Stage) = Function {
  funParameter ∷ FunctionParameter s,
  funReturn ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (Function s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (Function s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (Function s)


--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock (s ∷ Stage) = WhereBlock {
  whereClauses ∷ [WhereClause s]
  }
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (WhereBlock s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (WhereBlock s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (WhereBlock s)

data WhereClause (s ∷ Stage) =
  WhereOpenModule OpenModule
  | WhereTypeSig (TypeSignature s)
  | WhereFunClause (FunctionClause s)
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (WhereClause s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (WhereClause s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (WhereClause s)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

data Lambda (s ∷ Stage)
  = Lambda
      { lambdaParameters ∷ NonEmpty (PatternType s),
        lambdaBody ∷ ExpressionType s
      }
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (Lambda s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (Lambda s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (Lambda s)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application
  = Application
      { applicationFun ∷ ExpressionType 'Scoped,
        applicationArg ∷ ExpressionType 'Scoped
      }
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

newtype LetBlock (s ∷ Stage) = LetBlock [LetClause s]
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (LetBlock s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (LetBlock s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (LetBlock s)

data LetClause (s ∷ Stage) =
  LetTypeSig (TypeSignature s)
  | LetDefinition (FunctionClause s)
deriving stock instance (Show (PatternType s), Show (ExpressionType s)) ⇒ Show (LetClause s)
deriving stock instance (Eq (PatternType s), Eq (ExpressionType s)) ⇒ Eq (LetClause s)
deriving stock instance (Ord (PatternType s), Ord (ExpressionType s)) ⇒ Ord (LetClause s)

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

newtype Eval (s ∷ Stage) = Eval {
  evalExpression ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (Eval s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (Eval s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (Eval s)

newtype Print (s ∷ Stage) = Print {
  printExpression ∷ ExpressionType s
  }
deriving stock instance Show (ExpressionType s) ⇒ Show (Print s)
deriving stock instance Eq (ExpressionType s) ⇒ Eq (Print s)
deriving stock instance Ord (ExpressionType s) ⇒ Ord (Print s)
