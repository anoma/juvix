module MiniJuvix.Translation.MonoJuvixToMiniC where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import MiniJuvix.Internal.Strings
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Concrete.Language qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as S
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MiniC.Language
import MiniJuvix.Syntax.MiniC.Serialization
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Syntax.NameId
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono

newtype MiniCResult = MiniCResult
  { _resultCCode :: Text
  }

makeLenses ''MiniCResult

entryMiniC ::
  Mono.MonoJuvixResult ->
  Sem r MiniCResult
entryMiniC i = return (MiniCResult (serialize cunitResult))
  where
    compileInfo :: Mono.CompileInfoTable
    compileInfo = Mono.compileInfoTable i
    cunitResult :: CCodeUnit
    cunitResult =
      CCodeUnit
        { _ccodeCode = cheader <> cmodules
        }
    cheader :: [CCode]
    cheader =
      map
        ExternalMacro
        [ CppIncludeSystem stdlib,
          CppIncludeSystem stdbool,
          CppIncludeSystem stdio
        ]
    cmodules :: [CCode]
    cmodules = toList (i ^. Mono.resultModules) >>= (run . runReader compileInfo . goModule)

type Err = Text

unsupported :: Err -> a
unsupported msg = error (msg <> " Mono to C: not yet supported")

goModule ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.Module ->
  Sem r [CCode]
goModule Mono.Module {..} = goModuleBody _moduleBody

goModuleBody ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.ModuleBody ->
  Sem r [CCode]
goModuleBody Mono.ModuleBody {..} =
  concatMapM goStatement _moduleStatements

goStatement ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.Statement ->
  Sem r [CCode]
goStatement = \case
  Mono.StatementInductive d -> return (goInductiveDef d)
  Mono.StatementFunction d -> return (goFunctionDef d)
  Mono.StatementForeign d -> return (goForeign d)
  Mono.StatementAxiom d -> goAxiom d

type CTypeName = Text

asStruct :: Text -> Text
asStruct n = n <> "_s"

asTypeDef :: Text -> Text
asTypeDef n = n <> "_t"

asTag :: Text -> Text
asTag n = n <> "_tag"

asNew :: Text -> Text
asNew n = "new_" <> n

asNullary :: Text -> Text
asNullary n = n <> "_nullary"

asNewNullary :: Text -> Text
asNewNullary n = asNullary (asNew n)

asCast :: Text -> Text
asCast n = "as_" <> n

asIs :: Text -> Text
asIs n = "is_" <> n

asFunArg :: Text -> Text
asFunArg n = "fa" <> n

asCtorArg :: Text -> Text
asCtorArg n = "ca" <> n

mkArgs :: (Text -> Text) -> [Text]
mkArgs f = map (f . show) [0 :: Integer ..]

funArgs :: [Text]
funArgs = mkArgs asFunArg

ctorArgs :: [Text]
ctorArgs = mkArgs asCtorArg

mkName :: Mono.Name -> Text
mkName n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | T.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = T.filter isValidChar (n ^. Mono.nameText)
    isValidChar :: Char -> Bool
    isValidChar c = isLetter c && isAscii c
    adaptFirstLetter :: Text -> Text
    adaptFirstLetter t = case T.uncons t of
      Nothing -> impossible
      Just (h, r) -> T.cons (capitalize h) r
      where
        capitalize :: Char -> Char
        capitalize
          | capital = toUpper
          | otherwise = toLower
        capital = case n ^. Mono.nameKind of
          Mono.KNameConstructor -> True
          Mono.KNameInductive -> True
          Mono.KNameTopModule -> True
          Mono.KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Mono.nameKind of
      Mono.KNameTopModule -> mempty
      Mono.KNameFunction ->
        if n ^. Mono.nameText == main then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Mono.nameId . unNameId)

goFunctionDef :: Mono.FunctionDef -> [CCode]
goFunctionDef Mono.FunctionDef {..} =
  [ ExternalFunc
      ( Function
          { _funcReturnType = funReturnType ^. typeDeclType,
            _funcIsPtr = funReturnType ^. typeIsPtr,
            _funcQualifier = None,
            _funcName = funcName,
            _funcArgs = namedArgs asFunArg funArgTypes,
            _funcBody = maybeToList (BodyStatement <$> mkBody (goFunctionClause <$> toList _funDefClauses))
          }
      )
  ]
    <> (ExternalMacro . CppDefineParens <$> toList nullaryDefine)
  where
    mkBody :: [(Maybe Expression, Statement)] -> Maybe Statement
    mkBody cs = do
      let lastBranch = const fallback . head <$> nonEmpty cs
      foldr mkIf lastBranch cs
    mkIf :: (Maybe Expression, Statement) -> Maybe Statement -> Maybe Statement
    mkIf (mcondition, thenBranch) elseBranch = case mcondition of
      Nothing -> Just thenBranch
      Just condition ->
        Just
          ( StatementIf
              ( If
                  { _ifCondition = condition,
                    _ifThen = thenBranch,
                    _ifElse = elseBranch
                  }
              )
          )

    isNullary :: Bool
    isNullary = null funArgTypes && funcBasename /= main_
    funcBasename :: Text
    funcBasename = mkName _funDefName
    nullaryDefine :: Maybe Define
    nullaryDefine =
      if
          | isNullary ->
              Just $
                Define
                  { _defineName = funcBasename,
                    _defineBody = functionCall (ExpressionVar funcName) []
                  }
          | otherwise -> Nothing
    funcName =
      if
          | isNullary -> asNullary funcBasename
          | otherwise -> funcBasename
    funArgTypes :: [CDeclType]
    funArgTypes = fst funType
    funReturnType :: CDeclType
    funReturnType = snd funType
    funType :: ([CDeclType], CDeclType)
    funType = unfoldFunType' _funDefType
    unfoldFunType' :: Mono.Type -> ([CDeclType], CDeclType)
    unfoldFunType' = \case
      Mono.TypeFunction (Mono.Function l r) ->
        first (goType l :) (unfoldFunType' r)
      t -> ([], goType t)
    fallback :: Statement
    fallback =
      StatementCompound
        [ StatementExpr
            ( functionCall
                (ExpressionVar fprintf)
                [ ExpressionVar stderr_,
                  ExpressionLiteral (LiteralString "Error: Pattern match(es) are non-exhaustive in %s\n"),
                  ExpressionLiteral (LiteralString (_funDefName ^. Mono.nameText))
                ]
            ),
          StatementExpr
            ( functionCall
                (ExpressionVar exit)
                [ ExpressionVar exitFailure_
                ]
            )
        ]

type PatternBindings = HashMap Text Expression

goFunctionClause :: Mono.FunctionClause -> (Maybe Expression, Statement)
goFunctionClause Mono.FunctionClause {..} = (clauseCondition, returnStmt)
  where
    conditions :: [Expression]
    conditions = do
      (p, arg) <- zip _clausePatterns funArgs
      patternCondition (ExpressionVar arg) p

    patternCondition :: Expression -> Mono.Pattern -> [Expression]
    patternCondition arg = \case
      Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
        isCtor : subConditions
        where
          ctorName :: Text
          ctorName = mkName _constrAppConstructor
          isCtor :: Expression
          isCtor = functionCall (ExpressionVar (asIs ctorName)) [arg]
          asCtor :: Expression
          asCtor = functionCall (ExpressionVar (asCast ctorName)) [arg]
          subConditions :: [Expression]
          subConditions = do
            let subArgs = map (memberAccess Object asCtor) ctorArgs
            (p, subArg) <- zip _constrAppParameters subArgs
            patternCondition subArg p
      Mono.PatternVariable {} -> []
      Mono.PatternWildcard {} -> []

    clauseCondition :: Maybe Expression
    clauseCondition = fmap (foldr1 f) (nonEmpty conditions)
      where
        f :: Expression -> Expression -> Expression
        f e1 e2 =
          ExpressionBinary
            ( Binary
                { _binaryOp = And,
                  _binaryLeft = e1,
                  _binaryRight = e2
                }
            )
    patternBindings :: PatternBindings
    patternBindings = HashMap.fromList patternVars
    patternVars :: [(Text, Expression)]
    patternVars = do
      (p, arg) <- zipWith (curry (second ExpressionVar)) _clausePatterns funArgs
      case p of
        Mono.PatternVariable v -> [(v ^. Mono.nameText, arg)]
        Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
          goConstructorApp arg _constrAppConstructor _constrAppParameters
        Mono.PatternWildcard {} -> []
    returnStmt :: Statement
    returnStmt = StatementReturn (Just (run (runReader patternBindings (goExpression _clauseBody))))

goConstructorApp :: Expression -> Mono.Name -> [Mono.Pattern] -> [(Text, Expression)]
goConstructorApp arg n ps = do
  (p, field) <- zip ps ctorArgs
  let ctorField = memberAccess Object asConstructor field
  case p of
    Mono.PatternVariable v -> [(v ^. Mono.nameText, ctorField)]
    Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
      goConstructorApp ctorField _constrAppConstructor _constrAppParameters
    Mono.PatternWildcard {} -> []
  where
    asConstructor :: Expression
    asConstructor = functionCall (ExpressionVar (asCast (mkName n))) [arg]

goExpression :: Member (Reader PatternBindings) r => Mono.Expression -> Sem r Expression
goExpression = \case
  Mono.ExpressionIden i -> goIden i
  Mono.ExpressionApplication a -> goApplication a
  Mono.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))

goIden :: Member (Reader PatternBindings) r => Mono.Iden -> Sem r Expression
goIden = \case
  Mono.IdenFunction n -> return (ExpressionVar (mkName n))
  Mono.IdenConstructor n -> return (ExpressionVar (asNew (mkName n)))
  Mono.IdenVar n -> HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> ask
  Mono.IdenAxiom n -> return (ExpressionVar (mkName n))

goApplication :: forall r. Member (Reader PatternBindings) r => Mono.Application -> Sem r Expression
goApplication a = do
  (fName, fArgs) <- f
  return (functionCall fName (reverse fArgs))
  where
    f :: Sem r (Expression, [Expression])
    f = unfoldApp a
    unfoldApp :: Mono.Application -> Sem r (Expression, [Expression])
    unfoldApp Mono.Application {..} = case _appLeft of
      Mono.ExpressionApplication x -> do
        fName <- goExpression _appRight
        uf <- unfoldApp x
        return (second (fName :) uf)
      _ -> do
        fName <- goExpression _appLeft
        fArg <- goExpression _appRight
        return (fName, [fArg])

goLiteral :: C.LiteralLoc -> Literal
goLiteral C.LiteralLoc {..} = case _literalLocLiteral of
  C.LitString s -> LiteralString s
  C.LitInteger i -> LiteralInt i

goAxiom ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.AxiomDef ->
  Sem r [CCode]
goAxiom a = do
  backends <- lookupBackends (axiomName ^. Mono.nameId)
  case firstJust getCode backends of
    Nothing -> error ("C backend does not support this axiom:" <> show (axiomName ^. Mono.nameText))
    Just defineBody ->
      return
        [ ExternalMacro
            ( CppDefine
                ( Define
                    { _defineName = defineName,
                      _defineBody = ExpressionVar defineBody
                    }
                )
            )
        ]
  where
    axiomName :: Mono.Name
    axiomName = a ^. Mono.axiomName
    defineName :: Text
    defineName = mkName axiomName
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendC == b ^. backendItemBackend)
        $> b ^. backendItemCode
    lookupBackends ::
      Member (Reader Mono.CompileInfoTable) r =>
      NameId ->
      Sem r [BackendItem]
    lookupBackends f = (^. S.compileInfoBackendItems) . HashMap.lookupDefault impossible f <$> ask

goForeign :: ForeignBlock -> [CCode]
goForeign b = case b ^. foreignBackend of
  BackendC -> [Verbatim (b ^. foreignCode)]
  _ -> []

mkInductiveName :: Mono.InductiveDef -> Text
mkInductiveName i = mkName (i ^. Mono.inductiveName)

mkInductiveConstructorNames :: Mono.InductiveDef -> [Text]
mkInductiveConstructorNames i = mkName . view Mono.constructorName <$> i ^. Mono.inductiveConstructors

goInductiveDef :: Mono.InductiveDef -> [CCode]
goInductiveDef i =
  [ ExternalDecl structTypeDef,
    ExternalDecl tagsType
  ]
    <> (i ^. Mono.inductiveConstructors >>= goInductiveConstructorDef)
    <> [ExternalDecl inductiveDecl]
    <> (i ^. Mono.inductiveConstructors >>= goInductiveConstructorNew i)
    <> (ExternalFunc . isFunction <$> constructorNames)
    <> (ExternalFunc . asFunction <$> constructorNames)
  where
    baseName :: Text
    baseName = mkName (i ^. Mono.inductiveName)

    constructorNames :: [Text]
    constructorNames = mkInductiveConstructorNames i

    structTypeDef :: Declaration
    structTypeDef =
      typeDefWrap
        (asTypeDef baseName)
        ( DeclStructUnion
            ( StructUnion
                { _structUnionTag = StructTag,
                  _structUnionName = Just (asStruct baseName),
                  _structMembers = Nothing
                }
            )
        )

    tagsType :: Declaration
    tagsType =
      typeDefWrap
        (asTag baseName)
        ( DeclEnum
            ( Enum
                { _enumName = Nothing,
                  _enumMembers = Just (asTag <$> constructorNames)
                }
            )
        )

    inductiveDecl :: Declaration
    inductiveDecl =
      Declaration
        { _declType = inductiveStruct,
          _declIsPtr = False,
          _declName = Nothing,
          _declInitializer = Nothing
        }

    inductiveStruct :: DeclType
    inductiveStruct =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = StructTag,
              _structUnionName = Just (asStruct baseName),
              _structMembers =
                Just
                  [ typeDefType (asTag baseName) tag,
                    Declaration
                      { _declType = unionMembers,
                        _declIsPtr = False,
                        _declName = Just data_,
                        _declInitializer = Nothing
                      }
                  ]
            }
        )

    unionMembers :: DeclType
    unionMembers =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = UnionTag,
              _structUnionName = Nothing,
              _structMembers = Just (map (\ctorName -> typeDefType (asTypeDef ctorName) ctorName) constructorNames)
            }
        )

    isFunction :: Text -> Function
    isFunction ctorName =
      Function
        { _funcReturnType = BoolType,
          _funcIsPtr = False,
          _funcQualifier = StaticInline,
          _funcName = asIs ctorName,
          _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg],
          _funcBody =
            [ returnStatement
                ( equals
                    (memberAccess Pointer (ExpressionVar funcArg) tag)
                    (ExpressionVar (asTag ctorName))
                )
            ]
        }
      where
        funcArg :: Text
        funcArg = "a"

    asFunction :: Text -> Function
    asFunction ctorName =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef ctorName),
          _funcIsPtr = False,
          _funcQualifier = StaticInline,
          _funcName = asCast ctorName,
          _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg],
          _funcBody =
            [ returnStatement
                (memberAccess Object (memberAccess Pointer (ExpressionVar funcArg) data_) ctorName)
            ]
        }
      where
        funcArg :: Text
        funcArg = "a"

goInductiveConstructorNew ::
  Mono.InductiveDef ->
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorNew i ctor = ctorNewFun
  where
    ctorNewFun :: [CCode]
    ctorNewFun = if null ctorParams then ctorNewNullary else ctorNewNary

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    inductiveName :: Text
    inductiveName = mkInductiveName i

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

    ctorNewNullary :: [CCode]
    ctorNewNullary =
      [ ExternalFunc $
          commonFunctionDeclr
            (asNewNullary baseName)
            []
            [ BodyDecl allocInductive,
              BodyDecl (commonInitDecl (dataInit true_)),
              BodyStatement assignPtr,
              returnStatement (ExpressionVar tmpPtrName)
            ],
        ExternalMacro
          ( CppDefineParens
              ( Define
                  { _defineName = asNew baseName,
                    _defineBody = functionCall (ExpressionVar (asNewNullary baseName)) []
                  }
              )
          )
      ]

    ctorNewNary :: [CCode]
    ctorNewNary =
      [ ExternalFunc $
          commonFunctionDeclr
            (asNew baseName)
            ctorDecls
            [ BodyDecl allocInductive,
              BodyDecl ctorStructInit,
              BodyDecl (commonInitDecl (dataInit tmpCtorStructName)),
              BodyStatement assignPtr,
              returnStatement (ExpressionVar tmpPtrName)
            ]
      ]
      where
        ctorDecls :: [Declaration]
        ctorDecls = inductiveCtorArgs ctor

        ctorInit :: [DesigInit]
        -- TODO: _declName is never Nothing by construction, fix the types
        ctorInit = map (f . fromJust . (^. declName)) ctorDecls

        f :: Text -> DesigInit
        f fieldName =
          DesigInit
            { _desigDesignator = fieldName,
              _desigInitializer = ExprInitializer (ExpressionVar fieldName)
            }

        ctorStructInit :: Declaration
        ctorStructInit =
          Declaration
            { _declType = DeclTypeDefType (asTypeDef baseName),
              _declIsPtr = False,
              _declName = Just tmpCtorStructName,
              _declInitializer = Just (DesignatorInitializer ctorInit)
            }

    commonFunctionDeclr :: Text -> [Declaration] -> [BodyItem] -> Function
    commonFunctionDeclr name args body =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef inductiveName),
          _funcIsPtr = True,
          _funcQualifier = StaticInline,
          _funcName = name,
          _funcArgs = args,
          _funcBody = body
        }

    commonInitDecl :: Initializer -> Declaration
    commonInitDecl di =
      ( Declaration
          { _declType = DeclTypeDefType (asTypeDef inductiveName),
            _declIsPtr = False,
            _declName = Just tmpStructName,
            _declInitializer =
              Just
                ( DesignatorInitializer
                    [ DesigInit
                        { _desigDesignator = tag,
                          _desigInitializer = ExprInitializer (ExpressionVar (asTag baseName))
                        },
                      DesigInit
                        { _desigDesignator = data_,
                          _desigInitializer = di
                        }
                    ]
                )
          }
      )

    tmpPtrName :: Text
    tmpPtrName = "n"

    tmpStructName :: Text
    tmpStructName = "m"

    tmpCtorStructName :: Text
    tmpCtorStructName = "s"

    allocInductive :: Declaration
    allocInductive =
      ( Declaration
          { _declType = DeclTypeDefType (asTypeDef inductiveName),
            _declIsPtr = True,
            _declName = Just tmpPtrName,
            _declInitializer = Just (ExprInitializer (mallocSizeOf (asTypeDef inductiveName)))
          }
      )

    dataInit :: Text -> Initializer
    dataInit varName =
      DesignatorInitializer
        [ DesigInit
            { _desigDesignator = baseName,
              _desigInitializer = ExprInitializer (ExpressionVar varName)
            }
        ]

    assignPtr :: Statement
    assignPtr =
      StatementExpr
        ( ExpressionAssign
            ( Assign
                { _assignLeft =
                    ExpressionUnary
                      ( Unary
                          { _unaryOp = Indirection,
                            _unarySubject = ExpressionVar tmpPtrName
                          }
                      ),
                  _assignRight = ExpressionVar tmpStructName
                }
            )
        )

namedArgs :: (Text -> Text) -> [CDeclType] -> [Declaration]
namedArgs prefix = zipWith goTypeDecl argLabels
  where
    argLabels :: [Text]
    argLabels = prefix . show <$> [0 :: Integer ..]

inductiveCtorArgs :: Mono.InductiveConstructorDef -> [Declaration]
inductiveCtorArgs ctor = namedArgs asCtorArg (goType <$> ctorParams)
  where
    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

goInductiveConstructorDef ::
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorDef ctor =
  [ExternalDecl ctorDecl]
  where
    ctorDecl :: Declaration
    ctorDecl = if null ctorParams then ctorBool else ctorStruct

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

    ctorBool :: Declaration
    ctorBool = typeDefWrap (asTypeDef baseName) BoolType

    ctorStruct :: Declaration
    ctorStruct = typeDefWrap (asTypeDef baseName) struct

    struct :: DeclType
    struct =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = StructTag,
              _structUnionName = Just (asStruct baseName),
              _structMembers = Just (inductiveCtorArgs ctor)
            }
        )

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Mono.Type -> ([Mono.Type], Mono.Type)
unfoldFunType t = case t of
  Mono.TypeFunction (Mono.Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

goType :: Mono.Type -> CDeclType
goType t = case t of
  Mono.TypeIden ti -> getMonoType ti
  Mono.TypeFunction {} -> goTypeFunction (unfoldFunType t)
  Mono.TypeUniverse {} -> unsupported "TypeUniverse"
  Mono.TypeAny {} -> unsupported "TypeAny"
  where
    getMonoType :: Mono.TypeIden -> CDeclType
    getMonoType = \case
      Mono.TypeIdenInductive mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (asTypeDef (mkName mn)),
            _typeIsPtr = True
          }
      Mono.TypeIdenAxiom mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (mkName mn),
            _typeIsPtr = False
          }
    goTypeFunction :: ([Mono.Type], Mono.Type) -> CDeclType
    goTypeFunction (margs, mrType) =
      CDeclType
        { _typeDeclType =
            DeclFunPtr
              ( FunPtr
                  { _funPtrReturnType = rType ^. typeDeclType,
                    _funPtrIsPtr = rType ^. typeIsPtr,
                    _funPtrArgs = goType <$> margs
                  }
              ),
          _typeIsPtr = False
        }
      where
        rType :: CDeclType
        rType = goType mrType

goTypeDecl :: Text -> CDeclType -> Declaration
goTypeDecl n CDeclType {..} =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Just n,
      _declInitializer = Nothing
    }

mallocSizeOf :: Text -> Expression
mallocSizeOf typeName =
  functionCall (ExpressionVar malloc) [functionCall (ExpressionVar sizeof) [ExpressionVar typeName]]
