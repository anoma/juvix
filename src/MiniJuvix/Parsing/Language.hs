-- | Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix
module MiniJuvix.Parsing.Language where


import MiniJuvix.Utils.Prelude

newtype Symbol = Sym Text
  deriving stock (Show, Read, Eq)

type Name = NonEmpty Symbol

--------------------------------------------------------------------------------
-- Top level declaration
--------------------------------------------------------------------------------

data TopLevel
  = OperatorDef OperatorSyntaxDef
  | TypeSignatureDeclaration TypeSignature
  | DataTypeDeclaration DataType
  --- | RecordTypeDeclaration RecordType
  | ModuleDeclaration Module
  | OpenModuleDeclaration OpenModule
  | FunctionClause FunctionClause
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Fixity declaration
--------------------------------------------------------------------------------

type Precedence = Natural

type Operator = Name

data UnaryAssoc = PrefixOp | PostfixOp
  deriving stock (Show, Read, Eq)

data BinaryAssoc = None | LeftAssoc | RightAssoc
  deriving stock (Show, Read, Eq)

data OperatorArity =
  Unary {
   unaryAssoc :: UnaryAssoc
  }
  | Binary {
    binaryAssoc :: BinaryAssoc
  }
  deriving stock (Show, Read, Eq)

data OperatorSyntaxDef =
  OperatorSyntaxDef {
  opArity :: OperatorArity
  , opSymbol :: Operator
  , opPrecedence :: Int
  }
  deriving stock (Show, Read, Eq)

------------------------------------------------------------------------------
-- Type signature declaration
------------------------------------------------------------------------------

data TypeSignature
  = TypeSignature
      {
        sigName :: Name,
        sigQuantity :: Maybe Expression,
        sigType :: Expression
      }
  deriving stock (Show, Read, Eq)

-----------------------------------------------------------------------------
-- Data type construction declaration
-----------------------------------------------------------------------------

type DataConstructorName = Name

type DataTypeName = Name

data DataConstructorDef = DataConstructorDef {
  constructorName :: DataConstructorName
  , constructorType :: Expression
  }
  deriving stock (Show, Read, Eq)

data DataType
  = DataType
      { dataTypeName :: DataTypeName,
        dataTypeArgs :: [Expression],
        dataTypeConstructors :: [DataConstructorDef]
      }
  deriving stock (Show, Read, Eq)

------------------------------------------------------------------------------
-- Record type declaration
------------------------------------------------------------------------------

-- type RecordFieldName = Name

-- type RecordTypeName = Name

-- data RecordField = RecordField {
--   recordFieldName :: RecordFieldName,
--   recordType :: Expression
--   }
--   deriving stock (Show, Read, Eq)

-- data RecordType
--   = RecordType
--       { recordTypeName :: Name,
--         recordTypeConstructorName :: Name,
--         recordTypeArgs :: [Expression],
--         recordFields :: [RecordField]
--       }
--   deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type PreTermName = Name

-- newtype RecordFieldData = Set [(Name, Expression)]
--   deriving stock (Show, Read, Eq)

data Pattern
  = PatternName Name
  | PatternConstructor DataConstructorName [Pattern]
  --- | PatternRecord RecordTypeName RecordFieldData
  | PatternPreTerm PreTermName Name
  | PatternWildcard
  deriving stock (Show, Read, Eq)

data FunctionClause
  = FunClause
      { ownerFunction :: Name,
        clausePatterns :: [Pattern],
        clauseBody :: Expression,
        clauseWhere :: Maybe WhereBlock
      }
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data Module
  = Module
      { moduleName :: Name,
        moduleArgs :: [Expression],
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
  | LetBlockExpr LetBlock
  | OpenModuleExpr OpenModule
  | PreTermExpr PreTerm
  | UniverseExpr Universe
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Pre- types and terms (a.k.a primitive types and terms)
--------------------------------------------------------------------------------

newtype PreTerm = PreTerm PreTermName -- PreType should be here somehow?
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock = WhereBlock {
  whereClauses :: [WhereClause]
  }
  deriving stock (Show, Read, Eq)

data WhereClause =
  WhereOpenModule OpenModule
  | WhereTypeSig TypeSignature
  | WhereFunClause FunctionClause
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
      { applicationFun :: Expression,
        applicationArg :: Expression
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
