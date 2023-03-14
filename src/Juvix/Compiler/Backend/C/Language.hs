module Juvix.Compiler.Backend.C.Language where

import Juvix.Prelude.Base hiding (Enum)

newtype CCodeUnit = CCodeUnit
  { _ccodeCode :: [CCode]
  }

data CCode
  = ExternalDecl Declaration
  | ExternalFunc Function
  | ExternalFuncSig FunctionSig
  | ExternalMacro Cpp
  | Verbatim Text

--------------------------------------------------------------------------------
-- Prepreocessor Directives
--------------------------------------------------------------------------------

data Cpp
  = CppIncludeFile Text
  | CppIncludeSystem Text
  | CppDefine Define
  | CppDefineParens Define

data Define = Define
  { _defineName :: Text,
    _defineBody :: Expression
  }

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

data Declaration = Declaration
  { _declType :: DeclType,
    _declName :: Maybe Text,
    _declInitializer :: Maybe Initializer
  }
  deriving stock (Show, Eq)

data Initializer
  = ExprInitializer Expression
  | ListInitializer [Initializer]
  | DesignatorInitializer [DesigInit]
  deriving stock (Show, Eq)

data DesigInit = DesigInit
  { _desigDesignator :: Text,
    _desigInitializer :: Initializer
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

data FunctionSig = FunctionSig
  { _funcReturnType :: DeclType,
    _funcQualifier :: Qualifier,
    _funcName :: Text,
    _funcArgs :: [Declaration]
  }

data Function = Function
  { _funcSig :: FunctionSig,
    _funcBody :: [BodyItem]
  }

data BodyItem
  = BodyStatement Statement
  | BodyDecl Declaration

data Qualifier
  = StaticInline
  | None
  deriving stock (Eq)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data DeclType
  = DeclTypeDefType Text
  | DeclStructUnion StructUnion
  | DeclEnum Enum
  | DeclArray Array
  deriving stock (Show, Eq)

data StructUnion = StructUnion
  { _structUnionTag :: StructUnionTag,
    _structUnionName :: Maybe Text,
    _structMembers :: Maybe [Declaration]
  }
  deriving stock (Show, Eq)

data StructUnionTag
  = StructTag
  | UnionTag
  deriving stock (Show, Eq)

data Enum = Enum
  { _enumName :: Maybe Text,
    _enumMembers :: Maybe [Text]
  }
  deriving stock (Show, Eq)

data Array = Array
  { _arrayType :: DeclType,
    _arraySize :: Integer
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data Expression
  = ExpressionAssign Assign
  | ExpressionCall Call
  | ExpressionLiteral Literal
  | ExpressionVar Text
  | ExpressionBinary Binary
  | ExpressionUnary Unary
  | -- We need the "statement expression" GCC/clang extension because the
    -- Language.C library does not provide any special syntax for macros and we
    -- want to produce stuff like:
    -- PREALLOC(n, {STACK_PUSH(ARG(0));}, {STACK_POP(ARG(0));}).
    ExpressionStatement Statement
  deriving stock (Show, Eq)

data Assign = Assign
  { _assignLeft :: Expression,
    _assignRight :: Expression
  }
  deriving stock (Show, Eq)

data Call = Call
  { _callCallee :: Expression,
    _callArgs :: [Expression]
  }
  deriving stock (Show, Eq)

data Literal
  = LiteralInt Integer
  | LiteralChar Char
  | LiteralString Text
  deriving stock (Show, Eq)

data BinaryOp
  = Eq
  | Neq
  | And
  | Or
  | Plus
  deriving stock (Show, Eq)

data Binary = Binary
  { _binaryOp :: BinaryOp,
    _binaryLeft :: Expression,
    _binaryRight :: Expression
  }
  deriving stock (Show, Eq)

data UnaryOp
  = Address
  | Indirection
  | Negation
  deriving stock (Show, Eq)

data Unary = Unary
  { _unaryOp :: UnaryOp,
    _unarySubject :: Expression
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Statement
  = StatementReturn (Maybe Expression)
  | StatementIf If
  | StatementSwitch Switch
  | StatementLabel Label
  | StatementGoto Goto
  | StatementExpr Expression
  | StatementCompound [Statement]
  deriving stock (Show, Eq)

data If = If
  { _ifCondition :: Expression,
    _ifThen :: Statement,
    _ifElse :: Maybe Statement
  }
  deriving stock (Show, Eq)

data Switch = Switch
  { _switchCondition :: Expression,
    _switchCases :: [Case],
    _switchDefault :: Maybe Statement
  }
  deriving stock (Show, Eq)

data Case = Case
  { _caseValue :: Expression,
    _caseCode :: Statement
  }
  deriving stock (Show, Eq)

data Label = Label
  { _labelName :: Text,
    _labelCode :: Statement
  }
  deriving stock (Show, Eq)

newtype Goto = Goto
  { _gotoLabel :: Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Constructions
--------------------------------------------------------------------------------

functionCall :: Expression -> [Expression] -> Expression
functionCall fExpr args =
  ExpressionCall
    ( Call
        { _callCallee = fExpr,
          _callArgs = args
        }
    )

macroVar :: Text -> Expression
macroVar = ExpressionVar

macroCall :: Text -> [Expression] -> Expression
macroCall name = \case
  [] -> macroVar name
  args -> functionCall (ExpressionVar name) args

integer :: (Integral a) => a -> Expression
integer x = ExpressionLiteral (LiteralInt (fromIntegral x))

string :: Text -> Expression
string x = ExpressionLiteral (LiteralString x)

makeLenses ''CCodeUnit
makeLenses ''Declaration
makeLenses ''FunctionSig
makeLenses ''Function
