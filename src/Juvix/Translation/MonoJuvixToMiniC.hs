module Juvix.Translation.MonoJuvixToMiniC
  ( module Juvix.Translation.MonoJuvixToMiniC,
    module Juvix.Translation.MonoJuvixToMiniC.Types,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Builtins
import Juvix.Internal.Strings qualified as Str
import Juvix.Prelude
import Juvix.Syntax.Backends
import Juvix.Syntax.Concrete.Language qualified as C
import Juvix.Syntax.Concrete.Scoped.InfoTable qualified as S
import Juvix.Syntax.ForeignBlock
import Juvix.Syntax.MiniC.Language
import Juvix.Syntax.MiniC.Serialization
import Juvix.Syntax.MonoJuvix.Language qualified as Mono
import Juvix.Syntax.NameId
import Juvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import Juvix.Translation.MonoJuvixToMiniC.Base
import Juvix.Translation.MonoJuvixToMiniC.BuiltinTable
import Juvix.Translation.MonoJuvixToMiniC.Closure
import Juvix.Translation.MonoJuvixToMiniC.Types

entryMiniC :: forall r. Member Builtins r => Mono.MonoJuvixResult -> Sem r MiniCResult
entryMiniC i = MiniCResult . serialize <$> cunitResult
  where
    compileInfo :: Mono.CompileInfoTable
    compileInfo = Mono.compileInfoTable i

    cunitResult :: Sem r CCodeUnit
    cunitResult = do
      cmodules' <- cmodules
      return
        CCodeUnit
          { _ccodeCode = cheader <> cmodules'
          }

    cheader :: [CCode]
    cheader =
      map
        ExternalMacro
        [ CppIncludeSystem Str.stdbool,
          CppIncludeFile Str.minicRuntime
        ]

    cmodule :: Mono.Module -> Sem r [CCode]
    cmodule m = do
      let buildTable = Mono.buildTable m
      let defs =
            genStructDefs m
              <> run (runReader compileInfo (genAxioms m))
              <> run (runReader buildTable (genCTypes m))
              <> run (runReader buildTable (genFunctionSigs m))
              <> run (runReader buildTable (genClosures m))
      funDefs <- runReader buildTable (genFunctionDefs m)
      return (defs <> funDefs)

    cmodules :: Sem r [CCode]
    cmodules = concatMapM cmodule (toList (i ^. Mono.resultModules))

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

genCTypes :: forall r. Member (Reader Mono.InfoTable) r => Mono.Module -> Sem r [CCode]
genCTypes Mono.Module {..} =
  concatMapM go (_moduleBody ^. Mono.moduleStatements)
  where
    go :: Mono.Statement -> Sem r [CCode]
    go = \case
      Mono.StatementInductive d -> goInductiveDef d
      Mono.StatementAxiom {} -> return []
      Mono.StatementForeign d -> return (goForeign d)
      Mono.StatementFunction {} -> return []

genFunctionSigs :: forall r. Member (Reader Mono.InfoTable) r => Mono.Module -> Sem r [CCode]
genFunctionSigs Mono.Module {..} =
  concatMapM (applyOnFunStatement genFunctionSig) (_moduleBody ^. Mono.moduleStatements)

genFunctionDefs ::
  Members '[Reader Mono.InfoTable, Builtins] r =>
  Mono.Module ->
  Sem r [CCode]
genFunctionDefs Mono.Module {..} = genFunctionDefsBody _moduleBody

genFunctionDefsBody ::
  Members '[Reader Mono.InfoTable, Builtins] r =>
  Mono.ModuleBody ->
  Sem r [CCode]
genFunctionDefsBody Mono.ModuleBody {..} =
  concatMapM (applyOnFunStatement goFunctionDef) _moduleStatements

isNullary :: Text -> CFunType -> Bool
isNullary funName funType = null (funType ^. cFunArgTypes) && funName /= Str.main_

mkFunctionSig :: forall r. Member (Reader Mono.InfoTable) r => Mono.FunctionDef -> Sem r FunctionSig
mkFunctionSig Mono.FunctionDef {..} =
  cFunTypeToFunSig <$> funName <*> funType
  where
    -- Assumption: All clauses have the same number of patterns
    nPatterns :: Int
    nPatterns = length (head _funDefClauses ^. Mono.clausePatterns)

    baseFunType :: Sem r CFunType
    baseFunType = typeToFunType _funDefType

    funType :: Sem r CFunType
    funType = do
      typ <- baseFunType
      return
        ( if nPatterns == length (typ ^. cFunArgTypes)
            then typ
            else
              CFunType
                { _cFunArgTypes = take nPatterns (typ ^. cFunArgTypes),
                  _cFunReturnType = declFunctionPtrType
                }
        )

    funIsNullary :: Sem r Bool
    funIsNullary = isNullary funcBasename <$> funType

    funcBasename :: Text
    funcBasename = mkName _funDefName

    funName :: Sem r Text
    funName = bool funcBasename (asNullary funcBasename) <$> funIsNullary

genFunctionSig :: forall r. Member (Reader Mono.InfoTable) r => Mono.FunctionDef -> Sem r [CCode]
genFunctionSig d@(Mono.FunctionDef {..}) = do
  sig <- mkFunctionSig d
  nullaryDefine' <- nullaryDefine
  return
    ( [ExternalFuncSig sig]
        <> (ExternalMacro . CppDefineParens <$> toList nullaryDefine')
    )
  where
    nPatterns :: Int
    nPatterns = length (head _funDefClauses ^. Mono.clausePatterns)

    baseFunType :: Sem r CFunType
    baseFunType = typeToFunType _funDefType

    funType :: Sem r CFunType
    funType = do
      typ <- baseFunType
      return
        ( if nPatterns == length (typ ^. cFunArgTypes)
            then typ
            else
              CFunType
                { _cFunArgTypes = take nPatterns (typ ^. cFunArgTypes),
                  _cFunReturnType = declFunctionPtrType
                }
        )

    funIsNullary :: Sem r Bool
    funIsNullary = isNullary funcBasename <$> funType

    funcBasename :: Text
    funcBasename = mkName _funDefName

    funName :: Sem r Text
    funName = bool funcBasename (asNullary funcBasename) <$> funIsNullary

    nullaryDefine :: Sem r (Maybe Define)
    nullaryDefine = do
      n <- funName
      bool
        Nothing
        ( Just $
            Define
              { _defineName = funcBasename,
                _defineBody = functionCall (ExpressionVar n) []
              }
        )
        <$> funIsNullary

goFunctionDef ::
  Members '[Reader Mono.InfoTable, Builtins] r =>
  Mono.FunctionDef ->
  Sem r [CCode]
goFunctionDef d@(Mono.FunctionDef {..})
  | isJust _funDefBuiltin = return []
  | otherwise = do
      fc <- mapM (goFunctionClause (fst (unfoldFunType _funDefType))) (toList _funDefClauses)
      let bodySpec = fst <$> fc
      let preDecls :: [Function] = snd =<< fc
      funSig <- mkFunctionSig d
      return $
        (ExternalFunc <$> preDecls)
          <> [ ExternalFunc $
                 Function
                   { _funcSig = funSig,
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
                            <> _funDefName
                            ^. Mono.nameText
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
  Members '[Reader Mono.InfoTable, Builtins] r =>
  [Mono.Type] ->
  Mono.FunctionClause ->
  Sem r ((Maybe Expression, Statement), [Function])
goFunctionClause argTyps clause = do
  (stmt, decls) <- returnStmt
  cond <- clauseCondition
  return ((cond, stmt), decls)
  where
    conditions :: Sem r [Expression]
    conditions =
      concat
        <$> sequence
          [ patternCondition (ExpressionVar arg) p
            | (p, arg) <- zip (clause ^. Mono.clausePatterns) funArgs
          ]

    patternCondition :: Expression -> Mono.Pattern -> Sem r [Expression]
    patternCondition arg = \case
      Mono.PatternConstructorApp Mono.ConstructorApp {..} -> do
        ctorName <- getConstructorCName _constrAppConstructor

        let isCtor :: Expression
            isCtor = functionCall (ExpressionVar (asIs ctorName)) [arg]
            projCtor :: Text -> Expression
            projCtor ctorArg = functionCall (ExpressionVar (asProjName ctorArg ctorName)) [arg]
            subConditions :: Sem r [Expression]
            subConditions = fmap concat (zipWithM patternCondition (map projCtor ctorArgs) _constrAppParameters)
        fmap (isCtor :) subConditions
      Mono.PatternVariable {} -> return []
      Mono.PatternWildcard {} -> return []

    clauseCondition :: Sem r (Maybe Expression)
    clauseCondition = fmap (foldr1 f) . nonEmpty <$> conditions
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

goExpression :: Members '[Reader Mono.InfoTable, Builtins, Reader PatternInfoTable] r => Mono.Expression -> Sem r Expression
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
        (if null argTyps then goIden i else return $ functionCall (ExpressionVar evalFunName) [])
  Mono.ExpressionApplication a -> goApplication a
  Mono.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))

goIden :: Members '[Reader PatternInfoTable, Builtins, Reader Mono.InfoTable] r => Mono.Iden -> Sem r Expression
goIden = \case
  Mono.IdenFunction n -> do
    funInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoFunctions)
    let varName = case funInfo ^. Mono.functionInfoBuiltin of
          Just builtin -> fromJust (builtinFunctionName builtin)
          Nothing -> mkName n
    return (ExpressionVar varName)
  Mono.IdenConstructor n -> ExpressionVar <$> getConstructorCName n
  Mono.IdenVar n ->
    (^. bindingInfoExpr) . HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> asks (^. patternBindings)
  Mono.IdenAxiom n -> ExpressionVar <$> getAxiomCName n

goApplication :: forall r. Members '[Reader PatternInfoTable, Builtins, Reader Mono.InfoTable] r => Mono.Application -> Sem r Expression
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
                    (if null patternArgs then idenExp else functionCall idenExp patternArgs)
              return $ juvixFunctionCall callTyp funCall (drop nPatterns args)
          | otherwise -> do
              idenExp <- goIden iden
              return $ functionCall idenExp (reverse fArgs)
    Mono.IdenConstructor n -> returnFunCall iden fArgs n
    Mono.IdenAxiom n -> returnFunCall iden fArgs n
  where
    f :: Sem r (Mono.Iden, [Expression])
    f = unfoldApp a

    returnFunCall :: Mono.Iden -> [Expression] -> Mono.Name -> Sem r Expression
    returnFunCall iden fArgs name = do
      (idenType, _) <- getType iden
      ( if length fArgs < length (idenType ^. cFunArgTypes)
          then
            ( do
                let evalName = asEval (mkName name <> "_" <> show (length fArgs))
                return $ functionCall (ExpressionVar evalName) (reverse fArgs)
            )
          else
            ( do
                idenExp <- goIden iden
                return $ functionCall idenExp (reverse fArgs)
            )
        )

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
goLiteral l = case l ^. C.withLocParam of
  C.LitString s -> LiteralString s
  C.LitInteger i -> LiteralInt i

goAxiom ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.AxiomDef ->
  Sem r [CCode]
goAxiom a
  | isJust (a ^. Mono.axiomBuiltin) = return []
  | otherwise = do
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
    lookupBackends f = (^. S.compileInfoBackendItems) . HashMap.lookupDefault (error (show (a ^. Mono.axiomName))) f <$> ask

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

goInductiveDef :: Members '[Reader Mono.InfoTable] r => Mono.InductiveDef -> Sem r [CCode]
goInductiveDef i
  | isJust (i ^. Mono.inductiveBuiltin) = return []
  | otherwise = do
      ctorDefs <- concatMapM goInductiveConstructorDef (i ^. Mono.inductiveConstructors)
      ctorNews <- concatMapM (goInductiveConstructorNew i) (i ^. Mono.inductiveConstructors)
      projections <- concatMapM (goProjections inductiveTypeDef) (i ^. Mono.inductiveConstructors)
      return
        ( [ExternalDecl tagsType]
            <> ctorDefs
            <> [ExternalDecl inductiveDecl]
            <> ctorNews
            <> (ExternalFunc . isFunction <$> constructorNames)
            <> (ExternalFunc . asFunction <$> constructorNames)
            <> projections
        )
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

    inductiveTypeDef :: DeclType
    inductiveTypeDef = DeclTypeDefType (asTypeDef baseName)

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
                _funcArgs = [ptrType inductiveTypeDef funcArg]
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
                _funcArgs = [ptrType inductiveTypeDef funcArg]
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
  forall r.
  Members '[Reader Mono.InfoTable] r =>
  Mono.InductiveDef ->
  Mono.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorNew i ctor = ctorNewFun
  where
    ctorNewFun :: Sem r [CCode]
    ctorNewFun = if null ctorParams then return ctorNewNullary else ctorNewNary

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

    ctorNewNary :: Sem r [CCode]
    ctorNewNary = do
      ctorDecls' <- ctorDecls
      ctorStructInit' <- ctorStructInit
      return
        [ ExternalFunc $
            commonFunctionDeclr
              baseName
              ctorDecls'
              [ BodyDecl allocInductive,
                BodyDecl ctorStructInit',
                BodyDecl (commonInitDecl (dataInit tmpCtorStructName)),
                BodyStatement assignPtr,
                returnStatement (ExpressionVar tmpPtrName)
              ]
        ]
      where
        ctorDecls :: Sem r [Declaration]
        ctorDecls = inductiveCtorArgs ctor

        ctorInit :: Sem r [DesigInit]
        -- TODO: _declName is never Nothing by construction, fix the types
        ctorInit = map (f . fromJust . (^. declName)) <$> ctorDecls

        f :: Text -> DesigInit
        f fieldName =
          DesigInit
            { _desigDesignator = fieldName,
              _desigInitializer = ExprInitializer (ExpressionVar fieldName)
            }

        ctorStructInit :: Sem r Declaration
        ctorStructInit = do
          ctorInit' <- ctorInit
          return
            Declaration
              { _declType = DeclTypeDefType (asTypeDef baseName),
                _declIsPtr = False,
                _declName = Just tmpCtorStructName,
                _declInitializer = Just (DesignatorInitializer ctorInit')
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

inductiveCtorParams :: Members '[Reader Mono.InfoTable] r => Mono.InductiveConstructorDef -> Sem r [CDeclType]
inductiveCtorParams ctor = mapM goType (ctor ^. Mono.constructorParameters)

inductiveCtorArgs :: Members '[Reader Mono.InfoTable] r => Mono.InductiveConstructorDef -> Sem r [Declaration]
inductiveCtorArgs ctor = namedArgs asCtorArg <$> inductiveCtorParams ctor

goInductiveConstructorDef ::
  forall r.
  Members '[Reader Mono.InfoTable] r =>
  Mono.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorDef ctor = do
  d <- ctorDecl
  return [ExternalDecl d]
  where
    ctorDecl :: Sem r Declaration
    ctorDecl = if null ctorParams then return ctorBool else ctorStruct

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

    ctorBool :: Declaration
    ctorBool = typeDefWrap (asTypeDef baseName) BoolType

    ctorStruct :: Sem r Declaration
    ctorStruct = typeDefWrap (asTypeDef baseName) <$> struct

    struct :: Sem r DeclType
    struct = do
      args <- inductiveCtorArgs ctor
      return
        ( DeclStructUnion
            ( StructUnion
                { _structUnionTag = StructTag,
                  _structUnionName = Just (asStruct baseName),
                  _structMembers = Just args
                }
            )
        )

goProjections ::
  Members '[Reader Mono.InfoTable] r =>
  DeclType ->
  Mono.InductiveConstructorDef ->
  Sem r [CCode]
goProjections inductiveTypeDef ctor = do
  params <- inductiveCtorParams ctor
  return (ExternalFunc <$> zipWith projFunction [0 ..] params)
  where
    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    localName :: Text
    localName = "a"

    projFunction :: Int -> CDeclType -> Function
    projFunction argIdx projTyp =
      Function
        { _funcSig =
            FunctionSig
              { _funcReturnType = projTyp ^. typeDeclType,
                _funcIsPtr = projTyp ^. typeIsPtr,
                _funcQualifier = StaticInline,
                _funcName = asProj argIdx baseName,
                _funcArgs = [namedDecl localName True inductiveTypeDef]
              },
          _funcBody =
            [ BodyStatement
                ( StatementReturn
                    ( Just
                        ( memberAccess
                            Object
                            (functionCall (ExpressionVar (asCast baseName)) [ExpressionVar localName])
                            (asCtorArg (show argIdx))
                        )
                    )
                )
            ]
        }
