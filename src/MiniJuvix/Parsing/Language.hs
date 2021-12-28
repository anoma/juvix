-- | Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix
module MiniJuvix.Parsing.Language where


import MiniJuvix.Utils.Prelude


--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

newtype Symbol = Sym Text
  deriving stock (Show)

newtype ModulePath = ModulePath {
  path :: NonEmpty Symbol
  }
  deriving stock (Show)

data Qualified = Qualified {
  modulePath :: ModulePath,
  name :: Symbol
  }
  deriving stock (Show)

data Name =
  QualifiedName Qualified
  | Unqualified Symbol
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Top level declaration
--------------------------------------------------------------------------------

data Statement
  = StatementOperator OperatorSyntaxDef
  | StatementTypeSignature TypeSignature
  | StatementDataType DataTypeDef
  | StatementModule Module
  | StatementOpenModule OpenModule
  | StatementFunctionClause FunctionClause
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

type Precedence = Natural

data UnaryAssoc = PrefixOp | PostfixOp
  deriving stock (Show)

data BinaryAssoc = None | LeftAssoc | RightAssoc
  deriving stock (Show)

data OperatorArity =
  Unary {
   unaryAssoc :: UnaryAssoc
  }
  | Binary {
    binaryAssoc :: BinaryAssoc
  }
  deriving stock (Show)

data OperatorSyntaxDef =
  OperatorSyntaxDef {
  opArity :: OperatorArity
  , opSymbol :: Symbol
  , opPrecedence :: Int
  }
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Usage
-------------------------------------------------------------------------------

data Usage =
  UsageNone
  | UsageOnce
  | UsageAny
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature
  = TypeSignature
      {
        sigName :: Symbol,
        sigType :: Expression
      }
  deriving stock (Show)

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

type DataConstructorName = Symbol

type DataTypeName = Symbol

data DataConstructorDef = DataConstructorDef {
  constructorName :: DataConstructorName
  , constructorType :: Expression
  }
  deriving stock (Show)

data DataTypeDef
  = DataTypeDef
      { dataTypeName :: DataTypeName,
        dataTypeArgs :: [Expression],
        dataTypeConstructors :: [DataConstructorDef]
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

data Pattern
  = PatternVariable Symbol
  | PatternConstructor DataConstructorName [Pattern]
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show)

data FunctionClause
  = FunClause
      { ownerFunction :: Symbol,
        clausePatterns :: [Pattern],
        clauseBody :: Expression,
        clauseWhere :: Maybe WhereBlock
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data Module
  = Module
      { moduleName :: Symbol,
        body :: [Statement]
      }
  deriving stock (Show)

newtype OpenModule
  = OpenModule Name
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = ExprIdentifier Name
  | ExprApplication Application
  | ExprLambda Lambda
  | ExprLetBlock LetBlock
  | ExprUniverse Universe
  | ExprFun Function
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe Natural
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunParameter = FunParameter {
  paramName :: Maybe Symbol,
  paramUsage :: Maybe Usage,
  paramType :: Expression
  }
  deriving stock (Show)

data Function = Function {
  funParameter :: FunParameter,
  funReturn :: Expression
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock = WhereBlock {
  whereClauses :: [WhereClause]
  }
  deriving stock (Show)

data WhereClause =
  WhereOpenModule OpenModule
  | WhereTypeSig TypeSignature
  | WhereFunClause FunctionClause
  deriving stock (Show)

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
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application
  = Application
      { applicationFun :: Expression,
        applicationArg :: Expression
      }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

newtype LetBlock = LetBlock [LetClause]
  deriving stock (Show)

data LetClause =
  LetTypeSig TypeSignature
  | LetDefinition FunctionClause
  deriving stock (Show)
