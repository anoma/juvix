{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module MiniJuvix.Syntax.Concrete.Language where

--------------------------------------------------------------------------------

import qualified Data.Kind as GHC
import Data.Singletons
import Language.Haskell.TH.Syntax (Lift)
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

type family
  ModulePathType (t :: ModuleIsTop) ::
    (res :: GHC.Type) | res -> t
  where
  ModulePathType 'ModuleTop = ModulePath
  ModulePathType 'ModuleLocal = Symbol

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

newtype Symbol = Sym Text
  deriving stock (Show, Eq, Ord, Lift)

instance Hashable Symbol where
  hashWithSalt i (Sym t) = hashWithSalt i t

-- A.B.C corresponds to ModulePath [A,B] C
data ModulePath = ModulePath
  { modulePathDir :: [Symbol],
    modulePathName :: Symbol
  }
  deriving stock (Show, Eq, Ord, Generic, Lift)

instance Hashable ModulePath

data QualifiedName = QualifiedName
  { qualifiedModulePath :: ModulePath,
    qualifiedNameSymbol :: Symbol
  }
  deriving stock (Show, Eq, Ord, Lift)

data Name
  = NameQualified QualifiedName
  | NameUnqualified Symbol
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

data Statement
  = StatementOperator OperatorSyntaxDef
  | StatementTypeSignature TypeSignature
  | StatementImport Import
  | StatementDataType DataTypeDef
  | StatementModule (Module 'ModuleLocal)
  | StatementOpenModule OpenModule
  | StatementFunctionClause FunctionClause
  | StatementAxiom AxiomDef
  | StatementEval Eval
  | StatementPrint Print
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

newtype Import = Import
  { importModule :: ModulePath
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

type Precedence = Natural

data UnaryAssoc = AssocPrefix | AssocPostfix
  deriving stock (Show, Eq, Ord, Lift)

data BinaryAssoc = AssocNone | AssocLeft | AssocRight
  deriving stock (Show, Eq, Ord, Lift)

data OperatorArity
  = Unary
      { unaryAssoc :: UnaryAssoc
      }
  | Binary
      { binaryAssoc :: BinaryAssoc
      }
  deriving stock (Show, Eq, Ord, Lift)

data Fixity = Fixity
  { fixityPrecedence :: Precedence,
    fixityArity :: OperatorArity
  }
  deriving stock (Show, Eq, Ord, Lift)

data OperatorSyntaxDef = OperatorSyntaxDef
  { opSymbol :: Symbol,
    opFixity :: Fixity
  }
  deriving stock (Show, Eq, Ord, Lift)

-------------------------------------------------------------------------------
-- Quantity
-------------------------------------------------------------------------------

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord, Lift)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature = TypeSignature
  { sigName :: Symbol,
    sigType :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef = AxiomDef
  { axiomName :: Symbol,
    axiomType :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

-------------------------------------------------------------------------------
-- Lift type construction declaration
-------------------------------------------------------------------------------

type DataConstructorName = Symbol

type DataTypeName = Symbol

data DataConstructorDef = DataConstructorDef
  { constructorName :: DataConstructorName,
    constructorType :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

data DataTypeParameter = DataTypeParameter
  { dataTypeParameterName :: Symbol,
    dataTypeParameterType :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

data DataTypeDef = DataTypeDef
  { dataTypeName :: DataTypeName,
    dataTypeParameters :: [DataTypeParameter],
    dataTypeType :: Maybe (ExpressionSections),
    dataTypeConstructors :: [DataConstructorDef]
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternSection
  = PatternSectionName Name
  | PatternSectionWildcard
  | PatternSectionEmpty
  | PatternSectionParen PatternSections
  deriving stock (Show, Eq, Ord, Lift)

newtype PatternSections
  = PatternSections (NonEmpty PatternSection)
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type FunctionName = Symbol

data FunctionClause = FunClause
  { clauseOwnerFunction :: FunctionName,
    clausePatterns :: [PatternSection],
    clauseBody :: ExpressionSections,
    clauseWhere :: Maybe WhereBlock
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data ModuleIsTop = ModuleTop | ModuleLocal

-- The following Singleton related definitions could be scrapped if we depended
-- on the singletons-th library.
data SModuleIsTop (t :: ModuleIsTop) where
  SModuleTop :: SModuleIsTop 'ModuleTop
  SModuleLocal :: SModuleIsTop 'ModuleLocal

type instance Sing = SModuleIsTop

instance SingI 'ModuleTop where
  sing = SModuleTop

instance SingI 'ModuleLocal where
  sing = SModuleLocal

data Module (t :: ModuleIsTop) = Module
  { moduleModulePath :: ModulePathType t,
    moduleBody :: [Statement]
  }

deriving stock instance
  ( Show (ModulePathType t)
  ) =>
  Show (Module t)

deriving stock instance
  ( Eq (ModulePathType t)
  ) =>
  Eq (Module t)

deriving stock instance
  ( Ord (ModulePathType t)
  ) =>
  Ord (Module t)

deriving stock instance
  ( Lift (ModulePathType t)
  ) =>
  Lift (Module t)

data UsingHiding
  = Using (NonEmpty Symbol)
  | Hiding (NonEmpty Symbol)
  deriving stock (Show, Eq, Ord, Lift)

data OpenModule = OpenModule
  { openModuleName :: ModulePath,
    openUsingHiding :: Maybe UsingHiding
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Expression section
--------------------------------------------------------------------------------

-- | Expressions without application
data ExpressionSection
  = SectionIdentifier Name
  | SectionLambda Lambda
  | SectionLetBlock LetBlock
  | SectionUniverse Universe
  | SectionFunction Function
  | SectionFunArrow
  | SectionMatch Match
  | SectionParens ExpressionSections
  deriving stock (Show, Eq, Ord, Lift)

-- | Expressions without application
newtype ExpressionSections
  = ExpressionSections (NonEmpty ExpressionSection)
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

data MatchAlt = MatchAlt
  { matchAltPattern :: PatternSection,
    matchAltBody :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

data Match = Match
  { matchExpression :: ExpressionSections,
    matchAlts :: [MatchAlt]
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunctionParameter = FunctionParameter
  { paramName :: Maybe Symbol,
    paramUsage :: Maybe Usage,
    paramType :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

data Function = Function
  { funParameter :: FunctionParameter,
    funReturn :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock = WhereBlock
  { whereClauses :: [WhereClause]
  }
  deriving stock (Show, Eq, Ord, Lift)

data WhereClause
  = WhereOpenModule OpenModule
  | WhereTypeSig TypeSignature
  | WhereFunClause FunctionClause
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

data Lambda = Lambda
  { lambdaClauses :: [LambdaClause] }
  deriving stock (Show, Eq, Ord, Lift)

data LambdaClause = LambdaClause
  { lambdaParameters :: NonEmpty PatternSection,
    lambdaBody :: ExpressionSections
  }
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

newtype LetBlock = LetBlock {letClauses :: [LetClause]}
  deriving stock (Show, Eq, Ord, Lift)

data LetClause
  = LetTypeSig TypeSignature
  | LetFunClause FunctionClause
  deriving stock (Show, Eq, Ord, Lift)

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

newtype Eval = Eval {evalExpression :: ExpressionSections}
  deriving stock (Show, Eq, Ord, Lift)

newtype Print = Print {printExpression :: ExpressionSections}
  deriving stock (Show, Eq, Ord, Lift)
