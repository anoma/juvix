{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix
module MiniJuvix.Parsing.Language where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

newtype Symbol = Sym Text
  deriving stock (Show, Read, Eq)

type Name = NonEmpty Symbol

--------------------------------------------------------------------------------
-- File header
--------------------------------------------------------------------------------

data FileHeader topLevel
  = FileHeader Name [topLevel]
  | NoFileHeader [topLevel]
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Top level declaration
--------------------------------------------------------------------------------

data TopLevel
  = FixityDeclaration Fixity
  | TypeSignatureDeclaration TypeSignature
  | DataTypeDeclaration DataType
  | RecordTypeDeclaration RecordType
  | ModuleDeclaration Module
  | OpenModuleDeclaration OpenModule
  | FunctionDeclaration Function
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Fixity declaration
--------------------------------------------------------------------------------

type Precedence = Natural

type Operator = Name

data FixityMode
  = NonAssociative Operator Precedence
  | LeftAssociative Operator Precedence
  | RightAssociative Operator Precedence
  deriving stock (Show, Read, Eq)

newtype Fixity = Fixity FixityMode
  deriving stock (Show, Read, Eq)

------------------------------------------------------------------------------
-- Type signature declaration
--------------------------------------------------------------------------------

data TypeSignature
  = TypeSignature
      { termName :: Name,
        termQuantity :: Maybe Expression,
        termContext :: [Expression],
        termType :: Expression
      }
  deriving stock (Show, Read, Eq)

-----------------------------------------------------------------------------
-- Data type construction declaration
------------------------------------------------------------------------------

type DataConstructorName = Name

type DataTypeName = Name

data DataConstructor
  = DataConstructor DataTypeName DataConstructorName Expression
  deriving stock (Show, Read, Eq)

data DataType
  = DataType
      { dataTypeName :: DataTypeName,
        dataTypeParameters :: [Expression],
        dataTypeConstructors :: [DataConstructor]
      }
  deriving stock (Show, Read, Eq)

------------------------------------------------------------------------------
-- Record type declaration
------------------------------------------------------------------------------

type RecordFieldName = Name

type RecordTypeName = Name

data RecordField = RecordField RecordFieldName RecordTypeName Expression
  deriving stock (Show, Read, Eq)

data RecordType
  = RecordType
      { recordTypeName :: Name,
        recordTypeConstructorName :: Name,
        recordTypeParameters :: [Expression],
        recordFields :: [RecordField]
      }
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type PreTypeName = Name

newtype RecordFieldData = Set [(Name, Expression)]
  deriving stock (Show, Read, Eq)

data Pattern
  = PatternName Name
  | PatternData DataConstructorName [Pattern]
  | PatternRecord RecordTypeName RecordFieldData
  | PatternPreTerm PreTypeName Name
  deriving stock (Show, Read, Eq)

data FunctionClause
  = FunctionClause
      { ownerFunction :: Name,
        clausePatterns :: [Pattern],
        clauseBody :: Expression
      }
  deriving stock (Show, Read, Eq)

data Function
  = Function
      { functionName :: Name,
        clauses :: [FunctionClause]
      }
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data Module
  = Module
      { moduleName :: Name,
        moduleParameters :: [Expression],
        body :: [TopLevel]
      }
  deriving stock (Show, Read, Eq)

newtype OpenModule
  = OpenModule Name
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = IdentifierExpr Name
  | ApplicationExpr Application
  | LambdaExpr Lambda
  | PatternExpr Pattern
  | WhereBlockExpr WhereBlock
  | LetBlockExpr LetBlock
  | ModuleExpr Module
  | OpenModuleExpr OpenModule
  | PreTypeExpr PreType
  | PreTermExpr PreTerm
  | UniverseExpr Universe
  | Parened Expression
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Pre- types and terms (a.k.a primitive types and terms)
--------------------------------------------------------------------------------

newtype PreType = PreType TypeSignature
  deriving stock (Show, Read, Eq)

newtype PreTerm = PreTerm TypeSignature -- PreType should be here somehow?
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Where block expression
--------------------------------------------------------------------------------

newtype WhereBlock = WhereBlock {blockExpressions :: [Expression]}
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

data Lambda
  = Lambda
      { lambdaArguments :: [Pattern],
        lambdaBody :: Expression
      }
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application
  = Application
      { applicationName :: Expression,
        applicationArgs :: NonEmpty Expression
      }
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

newtype LetBlock = LetBlock Expression
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Infix expression
--------------------------------------------------------------------------------

data Infix
  = Infix
      { leftPart :: Expression,
        infixOp :: Name,
        rightPart :: Expression
      }
  deriving stock (Show, Read, Eq)
