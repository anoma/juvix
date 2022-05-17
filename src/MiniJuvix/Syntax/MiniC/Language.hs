module MiniJuvix.Syntax.MiniC.Language where

import MiniJuvix.Prelude hiding (Enum)

newtype CCodeUnit = CCodeUnit
  { _ccodeCode :: [CCode]
  }

data CCode
  = ExternalDecl Declaration
  | ExternalFunc Function
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
    _declIsPtr :: Bool,
    _declName :: Maybe Text,
    _declInitializer :: Maybe Initializer
  }

data Initializer
  = ExprInitializer Expression
  | DesignatorInitializer [DesigInit]

data DesigInit = DesigInit
  { _desigDesignator :: Text,
    _desigInitializer :: Initializer
  }

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

data Function = Function
  { _funcReturnType :: DeclType,
    _funcIsPtr :: Bool,
    _funcQualifier :: Qualifier,
    _funcName :: Text,
    _funcArgs :: [Declaration],
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
  | DeclTypeDef DeclType
  | DeclEnum Enum
  | DeclFunPtr FunPtr
  | BoolType

data StructUnion = StructUnion
  { _structUnionTag :: StructUnionTag,
    _structUnionName :: Maybe Text,
    _structMembers :: Maybe [Declaration]
  }

data StructUnionTag
  = StructTag
  | UnionTag

data Enum = Enum
  { _enumName :: Maybe Text,
    _enumMembers :: Maybe [Text]
  }

data FunPtr = FunPtr
  { _funPtrReturnType :: DeclType,
    _funPtrIsPtr :: Bool,
    _funPtrArgs :: [CDeclType]
  }

data CDeclType = CDeclType
  { _typeDeclType :: DeclType,
    _typeIsPtr :: Bool
  }

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data Expression
  = ExpressionAssign Assign
  | ExpressionCast Cast
  | ExpressionCall Call
  | ExpressionLiteral Literal
  | ExpressionVar Text
  | ExpressionBinary Binary
  | ExpressionUnary Unary
  | ExpressionMember MemberAccess

data Assign = Assign
  { _assignLeft :: Expression,
    _assignRight :: Expression
  }

data Cast = Cast
  { _castDecl :: Declaration,
    _castExpression :: Expression
  }

data Call = Call
  { _callCallee :: Expression,
    _callArgs :: [Expression]
  }

data Literal
  = LiteralInt Integer
  | LiteralChar Char
  | LiteralString Text

data BinaryOp
  = Eq
  | Neq
  | And
  | Or

data Binary = Binary
  { _binaryOp :: BinaryOp,
    _binaryLeft :: Expression,
    _binaryRight :: Expression
  }

data UnaryOp
  = Address
  | Indirection
  | Negation

data Unary = Unary
  { _unaryOp :: UnaryOp,
    _unarySubject :: Expression
  }

data MemberAccessOp
  = Object
  | Pointer
  deriving stock (Eq)

data MemberAccess = MemberAccess
  { _memberSubject :: Expression,
    _memberField :: Text,
    _memberOp :: MemberAccessOp
  }

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Statement
  = StatementReturn (Maybe Expression)
  | StatementIf If
  | StatementExpr Expression
  | StatementCompound [Statement]

data If = If
  { _ifCondition :: Expression,
    _ifThen :: Statement,
    _ifElse :: Maybe Statement
  }

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

ptrType :: DeclType -> Text -> Declaration
ptrType typ n =
  Declaration
    { _declType = typ,
      _declIsPtr = True,
      _declName = Just n,
      _declInitializer = Nothing
    }

typeDefType :: Text -> Text -> Declaration
typeDefType typName declName =
  Declaration
    { _declType = DeclTypeDefType typName,
      _declIsPtr = False,
      _declName = Just declName,
      _declInitializer = Nothing
    }

equals :: Expression -> Expression -> Expression
equals e1 e2 =
  ExpressionBinary
    ( Binary
        { _binaryOp = Eq,
          _binaryLeft = e1,
          _binaryRight = e2
        }
    )

memberAccess :: MemberAccessOp -> Expression -> Text -> Expression
memberAccess op e fieldName =
  ExpressionMember
    ( MemberAccess
        { _memberSubject = e,
          _memberField = fieldName,
          _memberOp = op
        }
    )

staticInlineFunc :: DeclType -> Bool -> Text -> [Declaration] -> [BodyItem] -> Function
staticInlineFunc t isPtr name args body =
  Function
    { _funcReturnType = t,
      _funcIsPtr = isPtr,
      _funcQualifier = StaticInline,
      _funcName = name,
      _funcArgs = args,
      _funcBody = body
    }

typeDefWrap :: Text -> DeclType -> Declaration
typeDefWrap typeDefName typ =
  Declaration
    { _declType = DeclTypeDef typ,
      _declIsPtr = False,
      _declName = Just typeDefName,
      _declInitializer = Nothing
    }

returnStatement :: Expression -> BodyItem
returnStatement e =
  BodyStatement (StatementReturn (Just e))

makeLenses ''CCodeUnit
makeLenses ''Declaration
makeLenses ''CDeclType
