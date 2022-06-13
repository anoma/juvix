module MiniJuvix.Syntax.MiniC.Language where

import MiniJuvix.Prelude hiding (Enum)

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
    _declIsPtr :: Bool,
    _declName :: Maybe Text,
    _declInitializer :: Maybe Initializer
  }
  deriving stock (Show, Eq)

data Initializer
  = ExprInitializer Expression
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
    _funcIsPtr :: Bool,
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
  | DeclTypeDef DeclType
  | DeclEnum Enum
  | DeclFunPtr FunPtr
  | DeclMiniJuvixClosure
  | BoolType
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

data FunPtr = FunPtr
  { _funPtrReturnType :: DeclType,
    _funPtrIsPtr :: Bool,
    _funPtrArgs :: [CDeclType]
  }
  deriving stock (Show, Eq)

data CDeclType = CDeclType
  { _typeDeclType :: DeclType,
    _typeIsPtr :: Bool
  }
  deriving stock (Show, Eq)

uIntPtrType :: DeclType
uIntPtrType = DeclTypeDefType "uintptr_t"

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
  deriving stock (Show, Eq)

data Assign = Assign
  { _assignLeft :: Expression,
    _assignRight :: Expression
  }
  deriving stock (Show, Eq)

data Cast = Cast
  { _castDecl :: Declaration,
    _castExpression :: Expression
  }
  deriving stock (Show, Eq)

castToType :: CDeclType -> Expression -> Expression
castToType cDecl e =
  ExpressionCast
    ( Cast
        { _castDecl = cDeclToDecl cDecl,
          _castExpression = e
        }
    )

cDeclToDecl :: CDeclType -> Declaration
cDeclToDecl CDeclType {..} =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Nothing,
      _declInitializer = Nothing
    }

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

data MemberAccessOp
  = Object
  | Pointer
  deriving stock (Show, Eq)

data MemberAccess = MemberAccess
  { _memberSubject :: Expression,
    _memberField :: Text,
    _memberOp :: MemberAccessOp
  }
  deriving stock (Show, Eq)

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
    { _funcSig =
        FunctionSig
          { _funcReturnType = t,
            _funcIsPtr = isPtr,
            _funcQualifier = StaticInline,
            _funcName = name,
            _funcArgs = args
          },
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
makeLenses ''FunctionSig
makeLenses ''Function
