module MiniJuvix.Translation.MonoJuvixToMiniC
  ( module MiniJuvix.Translation.MonoJuvixToMiniC,
    module MiniJuvix.Translation.MonoJuvixToMiniC.Types,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Internal.Strings qualified as Str
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
import MiniJuvix.Translation.MonoJuvixToMiniC.Base
import MiniJuvix.Translation.MonoJuvixToMiniC.Closure
import MiniJuvix.Translation.MonoJuvixToMiniC.Types

entryMiniC :: Mono.MonoJuvixResult -> Sem r MiniCResult
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
        [ CppIncludeSystem Str.stdbool,
          CppIncludeFile Str.minicRuntime
        ]

    cmodules :: [CCode]
    cmodules = do
      m <- toList (i ^. Mono.resultModules)
      let buildTable = Mono.buildTable m
      genStructDefs m
        <> run (runReader compileInfo (genAxioms m))
        <> genCTypes m
        <> genFunctionSigs m
        <> run (runReader buildTable (genClosures m))
        <> run (runReader buildTable (genFunctionDefs m))

genStructDefs :: Mono.Module -> [CCode]
genStructDefs Mono.Module {..} =
  concatMap go (_moduleBody ^. Mono.moduleStatements)
  where
    go :: Mono.Statement -> [CCode]
    go = \case
      Mono.StatementInductive d -> mkInductiveTypeDef d
      _ -> []

genAxioms :: forall r. Members '[Reader Mono.CompileInfoTable] r => Mono.Module -> Sem r [CCode]
genAxioms Mono.Module {..} =
  concatMapM go (_moduleBody ^. Mono.moduleStatements)
  where
    go :: Mono.Statement -> Sem r [CCode]
    go = \case
      Mono.StatementInductive {} -> return []
      Mono.StatementAxiom d -> goAxiom d
      Mono.StatementForeign {} -> return []
      Mono.StatementFunction {} -> return []

genCTypes :: Mono.Module -> [CCode]
genCTypes Mono.Module {..} =
  concatMap go (_moduleBody ^. Mono.moduleStatements)
  where
    go :: Mono.Statement -> [CCode]
    go = \case
      Mono.StatementInductive d -> goInductiveDef d
      Mono.StatementAxiom {} -> []
      Mono.StatementForeign d -> goForeign d
      Mono.StatementFunction {} -> []

genFunctionSigs :: Mono.Module -> [CCode]
genFunctionSigs Mono.Module {..} =
  applyOnFunStatement genFunctionSig =<< _moduleBody ^. Mono.moduleStatements

genFunctionDefs ::
  Members '[Reader Mono.InfoTable] r =>
  Mono.Module ->
  Sem r [CCode]
genFunctionDefs Mono.Module {..} = genFunctionDefsBody _moduleBody

genFunctionDefsBody ::
  Members '[Reader Mono.InfoTable] r =>
  Mono.ModuleBody ->
  Sem r [CCode]
genFunctionDefsBody Mono.ModuleBody {..} =
  concatMapM (applyOnFunStatement goFunctionDef) _moduleStatements

isNullary :: Text -> CFunType -> Bool
isNullary funName funType = null (funType ^. cFunArgTypes) && funName /= Str.main_

mkFunctionSig :: Mono.FunctionDef -> FunctionSig
mkFunctionSig Mono.FunctionDef {..} =
  cFunTypeToFunSig funName funType
  where
    -- Assumption: All clauses have the same number of patterns
    nPatterns :: Int
    nPatterns = length (head _funDefClauses ^. Mono.clausePatterns)

    baseFunType :: CFunType
    baseFunType = typeToFunType _funDefType

    funType :: CFunType
    funType =
      if
          | nPatterns == length (baseFunType ^. cFunArgTypes) -> baseFunType
          | otherwise ->
              CFunType
                { _cFunArgTypes = take nPatterns (baseFunType ^. cFunArgTypes),
                  _cFunReturnType = declFunctionPtrType
                }

    funIsNullary :: Bool
    funIsNullary = isNullary funcBasename funType

    funcBasename :: Text
    funcBasename = mkName _funDefName

    funName :: Text
    funName =
      if
          | funIsNullary -> asNullary funcBasename
          | otherwise -> funcBasename

genFunctionSig :: Mono.FunctionDef -> [CCode]
genFunctionSig d@(Mono.FunctionDef {..}) =
  [ExternalFuncSig (mkFunctionSig d)]
    <> (ExternalMacro . CppDefineParens <$> toList nullaryDefine)
  where
    nPatterns :: Int
    nPatterns = length (head _funDefClauses ^. Mono.clausePatterns)

    baseFunType :: CFunType
    baseFunType = typeToFunType _funDefType

    funType :: CFunType
    funType =
      if
          | nPatterns == length (baseFunType ^. cFunArgTypes) -> baseFunType
          | otherwise ->
              CFunType
                { _cFunArgTypes = take nPatterns (baseFunType ^. cFunArgTypes),
                  _cFunReturnType = declFunctionPtrType
                }

    funIsNullary :: Bool
    funIsNullary = isNullary funcBasename funType

    funcBasename :: Text
    funcBasename = mkName _funDefName

    funName :: Text
    funName =
      if
          | funIsNullary -> asNullary funcBasename
          | otherwise -> funcBasename

    nullaryDefine :: Maybe Define
    nullaryDefine =
      if
          | funIsNullary ->
              Just $
                Define
                  { _defineName = funcBasename,
                    _defineBody = functionCall (ExpressionVar funName) []
                  }
          | otherwise -> Nothing

goFunctionDef ::
  Members '[Reader Mono.InfoTable] r =>
  Mono.FunctionDef ->
  Sem r [CCode]
goFunctionDef d@(Mono.FunctionDef {..}) = do
  fc <- mapM (goFunctionClause (fst (unfoldFunType _funDefType))) (toList _funDefClauses)
  let bodySpec = fst <$> fc
  let preDecls :: [Function] = snd =<< fc
  return $
    (ExternalFunc <$> preDecls)
      <> [ ExternalFunc $
             Function
               { _funcSig = mkFunctionSig d,
                 _funcBody = maybeToList (BodyStatement <$> mkBody bodySpec)
               }
         ]
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

    fallback :: Statement
    fallback =
      StatementCompound
        [ StatementExpr
            ( functionCall
                (ExpressionVar Str.putStrLn_)
                [ ExpressionLiteral
                    ( LiteralString
                        ( "Error: Pattern match(es) are non-exhaustive in "
                            <> (_funDefName ^. Mono.nameText)
                        )
                    )
                ]
            ),
          StatementExpr
            ( functionCall
                (ExpressionVar Str.exit)
                [ ExpressionVar Str.exitFailure_
                ]
            )
        ]

goFunctionClause ::
  forall r.
  Members '[Reader Mono.InfoTable] r =>
  [Mono.Type] ->
  Mono.FunctionClause ->
  Sem r ((Maybe Expression, Statement), [Function])
goFunctionClause argTyps clause = do
  (stmt, decls) <- returnStmt
  return ((clauseCondition, stmt), decls)
  where
    conditions :: [Expression]
    conditions = do
      (p, arg) <- zip (clause ^. Mono.clausePatterns) funArgs
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
    returnStmt :: Sem r (Statement, [Function])
    returnStmt = do
      bindings <- buildPatternInfoTable argTyps clause
      (decls :: [Function], clauseResult) <- runOutputList (runReader bindings (goExpression (clause ^. Mono.clauseBody)))
      return (StatementReturn (Just clauseResult), decls)

goExpression :: Members '[Reader Mono.InfoTable, Reader PatternInfoTable] r => Mono.Expression -> Sem r Expression
goExpression = \case
  Mono.ExpressionIden i -> do
    let rootFunMonoName = Mono.getName i
        rootFunName = mkName rootFunMonoName
        evalFunName = asEval (rootFunName <> "_0")
    case i of
      Mono.IdenVar {} -> goIden i
      _ -> do
        (t, _) <- getType i
        let argTyps = t ^. cFunArgTypes
        if
            | null argTyps -> goIden i
            | otherwise -> return $ functionCall (ExpressionVar evalFunName) []
  Mono.ExpressionApplication a -> goApplication a
  Mono.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))

goIden :: Members '[Reader PatternInfoTable, Reader Mono.InfoTable] r => Mono.Iden -> Sem r Expression
goIden = \case
  Mono.IdenFunction n -> return (ExpressionVar (mkName n))
  Mono.IdenConstructor n -> return (ExpressionVar (mkName n))
  Mono.IdenVar n ->
    (^. bindingInfoExpr) . HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> asks (^. patternBindings)
  Mono.IdenAxiom n -> return (ExpressionVar (mkName n))

goApplication :: forall r. Members '[Reader PatternInfoTable, Reader Mono.InfoTable] r => Mono.Application -> Sem r Expression
goApplication a = do
  (iden, fArgs) <- f

  case iden of
    Mono.IdenVar n -> do
      BindingInfo {..} <- HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> asks (^. patternBindings)
      return $ juvixFunctionCall _bindingInfoType _bindingInfoExpr (reverse fArgs)
    Mono.IdenFunction n -> do
      nPatterns <- (^. Mono.functionInfoPatterns) . HashMap.lookupDefault impossible n <$> asks (^. Mono.infoFunctions)
      (idenType, _) <- getType iden
      let nArgTyps = length (idenType ^. cFunArgTypes)
      if
          | length fArgs < nArgTyps -> do
              let name = mkName (Mono.getName iden)
                  evalName = asEval (name <> "_" <> show (length fArgs))
              return $ functionCall (ExpressionVar evalName) (reverse fArgs)
          | nPatterns < nArgTyps -> do
              idenExp <- goIden iden
              let callTyp = idenType {_cFunArgTypes = drop nPatterns (idenType ^. cFunArgTypes)}
                  args = reverse fArgs
                  patternArgs = take nPatterns args
                  funCall =
                    if
                        | null patternArgs -> idenExp
                        | otherwise -> functionCall idenExp patternArgs
              return $ juvixFunctionCall callTyp funCall (drop nPatterns args)
          | otherwise -> do
              idenExp <- goIden iden
              return $ functionCall idenExp (reverse fArgs)
    _ -> do
      (idenType, _) <- getType iden
      if
          | (length fArgs < length (idenType ^. cFunArgTypes)) -> do
              let name = mkName (Mono.getName iden)
                  evalName = asEval (name <> "_" <> show (length fArgs))
              return $ functionCall (ExpressionVar evalName) (reverse fArgs)
          | otherwise -> do
              idenExp <- goIden iden
              return $ functionCall idenExp (reverse fArgs)
  where
    f :: Sem r (Mono.Iden, [Expression])
    f = unfoldApp a

    unfoldApp :: Mono.Application -> Sem r (Mono.Iden, [Expression])
    unfoldApp Mono.Application {..} = case _appLeft of
      Mono.ExpressionApplication x -> do
        fName <- goExpression _appRight
        uf <- unfoldApp x
        return (second (fName :) uf)
      Mono.ExpressionIden i -> do
        fArg <- goExpression _appRight
        return (i, [fArg])
      Mono.ExpressionLiteral {} -> impossible

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
        $> b
        ^. backendItemCode
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

mkInductiveTypeDef :: Mono.InductiveDef -> [CCode]
mkInductiveTypeDef i =
  [ExternalDecl structTypeDef]
  where
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

    baseName :: Text
    baseName = mkName (i ^. Mono.inductiveName)

goInductiveDef :: Mono.InductiveDef -> [CCode]
goInductiveDef i =
  [ ExternalDecl tagsType
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
                  [ typeDefType (asTag baseName) Str.tag,
                    Declaration
                      { _declType = unionMembers,
                        _declIsPtr = False,
                        _declName = Just Str.data_,
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
              _structMembers = Just (map (\ctorName -> typeDefType (asTypeDef ctorName) (asField ctorName)) constructorNames)
            }
        )

    isFunction :: Text -> Function
    isFunction ctorName =
      Function
        { _funcSig =
            FunctionSig
              { _funcReturnType = BoolType,
                _funcIsPtr = False,
                _funcQualifier = StaticInline,
                _funcName = asIs ctorName,
                _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg]
              },
          _funcBody =
            [ returnStatement
                ( equals
                    (memberAccess Pointer (ExpressionVar funcArg) Str.tag)
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
        { _funcSig =
            FunctionSig
              { _funcReturnType = DeclTypeDefType (asTypeDef ctorName),
                _funcIsPtr = False,
                _funcQualifier = StaticInline,
                _funcName = asCast ctorName,
                _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg]
              },
          _funcBody =
            [ returnStatement
                (memberAccess Object (memberAccess Pointer (ExpressionVar funcArg) Str.data_) (asField ctorName))
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
            (asNullary baseName)
            []
            [ BodyDecl allocInductive,
              BodyDecl (commonInitDecl (dataInit Str.true_)),
              BodyStatement assignPtr,
              returnStatement (ExpressionVar tmpPtrName)
            ],
        ExternalMacro
          ( CppDefineParens
              ( Define
                  { _defineName = baseName,
                    _defineBody = functionCall (ExpressionVar (asNullary baseName)) []
                  }
              )
          )
      ]

    ctorNewNary :: [CCode]
    ctorNewNary =
      [ ExternalFunc $
          commonFunctionDeclr
            baseName
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
        { _funcSig =
            FunctionSig
              { _funcReturnType = DeclTypeDefType (asTypeDef inductiveName),
                _funcIsPtr = True,
                _funcQualifier = StaticInline,
                _funcName = name,
                _funcArgs = args
              },
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
                        { _desigDesignator = Str.tag,
                          _desigInitializer = ExprInitializer (ExpressionVar (asTag baseName))
                        },
                      DesigInit
                        { _desigDesignator = Str.data_,
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
            { _desigDesignator = asField baseName,
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
