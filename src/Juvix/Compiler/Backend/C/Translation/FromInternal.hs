module Juvix.Compiler.Backend.C.Translation.FromInternal
  ( module Juvix.Compiler.Backend.C.Translation.FromInternal,
    module Juvix.Compiler.Backend.C.Data.Types,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Compiler.Abstract.Translation.FromConcrete.Data.Context qualified as Abstract
import Juvix.Compiler.Backend.C.Data.Base
import Juvix.Compiler.Backend.C.Data.BuiltinTable
import Juvix.Compiler.Backend.C.Data.Closure
import Juvix.Compiler.Backend.C.Data.Types
import Juvix.Compiler.Backend.C.Extra.Serialization
import Juvix.Compiler.Backend.C.Language
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoper
import Juvix.Compiler.Concrete.Language qualified as C
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Extra (mkPolyType')
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Trans
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Internal1
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding (Binary, Unary)

type CompileInfoTable = HashMap Scoper.NameId Scoper.CompileInfo

compileInfoTable :: Typed.InternalTypedResult -> CompileInfoTable
compileInfoTable r =
  HashMap.mapKeys
    (^. Scoper.nameId)
    ( r
        ^. Typed.resultInternalArityResult
          . Internal1.resultInternalResult
          . Internal.resultAbstract
          . Abstract.resultScoper
          . Scoper.resultScoperTable
          . Scoper.infoCompilationRules
    )

fromInternal ::
  forall r.
  Member Builtins r =>
  Typed.InternalTypedResult ->
  Sem r MiniCResult
fromInternal i = MiniCResult . serialize <$> cunitResult
  where
    compileInfo :: CompileInfoTable
    compileInfo = compileInfoTable i

    typesTable :: Typed.TypesTable
    typesTable = i ^. Typed.resultIdenTypes

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

    cmodule :: Internal.Module -> Sem r [CCode]
    cmodule m = do
      let infoTable = buildTable1 m
      let defs =
            genStructDefs m
              <> run (runReader compileInfo (runReader infoTable (genAxioms m)))
              <> run (runReader infoTable (genCTypes m))
              <> run (runReader infoTable (runReader typesTable (genFunctionSigs m)))
              <> run (runReader infoTable (runReader typesTable (genClosures m)))
      funDefs <- runReader infoTable (runReader typesTable (genFunctionDefs m))
      return (defs <> funDefs)

    cmodules :: Sem r [CCode]
    cmodules = concatMapM cmodule (toList (i ^. Typed.resultModules))

validWasmIdent :: Text -> Bool
validWasmIdent = T.all (\c -> c == '_' || isAlphaNum c)

genStructDefs :: Internal.Module -> [CCode]
genStructDefs Internal.Module {..} =
  concatMap go (_moduleBody ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> [CCode]
    go = \case
      Internal.StatementInductive d -> mkInductiveTypeDef d
      Internal.StatementInclude i ->
        concatMap go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)
      _ -> []

genAxioms :: forall r. Members '[Reader Internal.InfoTable, Reader CompileInfoTable] r => Internal.Module -> Sem r [CCode]
genAxioms Internal.Module {..} =
  concatMapM go (_moduleBody ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r [CCode]
    go = \case
      Internal.StatementInductive {} -> return []
      Internal.StatementAxiom d -> goAxiom d
      Internal.StatementForeign {} -> return []
      Internal.StatementFunction {} -> return []
      Internal.StatementInclude i ->
        concatMapM go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

genCTypes :: forall r. Member (Reader Internal.InfoTable) r => Internal.Module -> Sem r [CCode]
genCTypes Internal.Module {..} =
  concatMapM go (_moduleBody ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r [CCode]
    go = \case
      Internal.StatementInductive d -> goInductiveDef d
      Internal.StatementAxiom {} -> return []
      Internal.StatementForeign d -> return (goForeign d)
      Internal.StatementFunction {} -> return []
      Internal.StatementInclude i ->
        concatMapM go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

genFunctionSigs :: forall r. Members '[Reader Internal.InfoTable, Reader Typed.TypesTable] r => Internal.Module -> Sem r [CCode]
genFunctionSigs Internal.Module {..} =
  concatMapM (applyOnFunStatement genFunctionDef) (_moduleBody ^. Internal.moduleStatements)
  where
    genFunctionDef :: Internal.FunctionDef -> Sem r [CCode]
    genFunctionDef d
      | validWasmIdent nameText = (ExternalAttribute (ExportName nameText) :) <$> sig
      | otherwise = sig
      where
        nameText :: Text
        nameText = d ^. Internal.funDefName . Internal.nameText

        sig :: Sem r [CCode]
        sig = genFunctionSig d

genFunctionDefs ::
  Members '[Reader Internal.InfoTable, Reader Typed.TypesTable, Builtins] r =>
  Internal.Module ->
  Sem r [CCode]
genFunctionDefs Internal.Module {..} = genFunctionDefsBody _moduleBody

genFunctionDefsBody ::
  Members '[Reader Internal.InfoTable, Reader Typed.TypesTable, Builtins] r =>
  Internal.ModuleBody ->
  Sem r [CCode]
genFunctionDefsBody Internal.ModuleBody {..} =
  concatMapM (applyOnFunStatement goFunctionDef) _moduleStatements

isNullary :: Text -> CFunType -> Bool
isNullary funName funType = null (funType ^. cFunArgTypes) && funName /= Str.main_

mkFunctionSig :: forall r. Members '[Reader Internal.InfoTable, Reader Typed.TypesTable] r => Internal.FunctionDef -> Sem r FunctionSig
mkFunctionSig Internal.FunctionDef {..} =
  cFunTypeToFunSig <$> funName <*> funType
  where
    baseFunType :: Sem r CFunType
    baseFunType = typeToFunType (mkPolyType' _funDefType)

    funType :: Sem r CFunType
    funType = do
      -- Assumption: All clauses have the same number of patterns
      pats <- getClausePatterns (head _funDefClauses)
      let nPatterns = length pats
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

genFunctionSig :: forall r. Members '[Reader Internal.InfoTable, Reader Typed.TypesTable] r => Internal.FunctionDef -> Sem r [CCode]
genFunctionSig d@(Internal.FunctionDef {..}) = do
  sig <- mkFunctionSig d
  nullaryDefine' <- nullaryDefine
  return
    ( [ExternalFuncSig sig]
        <> (ExternalMacro . CppDefineParens <$> toList nullaryDefine')
    )
  where
    baseFunType :: Sem r CFunType
    baseFunType = typeToFunType (mkPolyType' _funDefType)

    funType :: Sem r CFunType
    funType = do
      pats <- getClausePatterns (head _funDefClauses)
      let nPatterns = length pats
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
  Members '[Reader Internal.InfoTable, Reader Typed.TypesTable, Builtins] r =>
  Internal.FunctionDef ->
  Sem r [CCode]
goFunctionDef d@(Internal.FunctionDef {..})
  | isJust _funDefBuiltin = return []
  | otherwise = do
      funSig <- mkFunctionSig d
      fc <- mapM (goFunctionClause funSig (fst (unfoldFunType (mkPolyType' _funDefType)))) (toList _funDefClauses)
      let bodySpec = fst <$> fc
      let preDecls :: [Function] = snd =<< fc
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
                (ExpressionVar Str.debug_)
                [ ExpressionLiteral
                    ( LiteralString
                        ( "Error: Pattern match(es) are non-exhaustive in "
                            <> _funDefName
                              ^. Internal.nameText
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
  Members '[Reader Internal.InfoTable, Reader Typed.TypesTable, Builtins] r =>
  FunctionSig ->
  [Internal.PolyType] ->
  Internal.FunctionClause ->
  Sem r ((Maybe Expression, Statement), [Function])
goFunctionClause funSig argTyps clause = do
  (stmt, decls) <- returnStmt
  cond <- clauseCondition
  return ((cond, stmt), decls)
  where
    conditions :: Sem r [Expression]
    conditions = do
      pats <- getClausePatterns clause
      concat
        <$> sequence
          [ patternCondition (ExpressionVar arg) p
            | (p, arg) <- zip pats funArgs
          ]

    patternCondition :: Expression -> Internal.Pattern -> Sem r [Expression]
    patternCondition arg = \case
      Internal.PatternConstructorApp Internal.ConstructorApp {..} -> do
        ctorName <- getConstructorCName _constrAppConstructor
        ty <- typeOfConstructor _constrAppConstructor

        let isCtor :: Expression
            isCtor = functionCall (ExpressionVar (asIs ctorName)) [castToType ty arg]
            projCtor :: Text -> Expression
            projCtor ctorArg = functionCall (ExpressionVar (asProjName ctorArg ctorName)) [castToType ty arg]
            subConditions :: Sem r [Expression]
            subConditions =
              fmap
                concat
                ( zipWithM
                    patternCondition
                    (map projCtor ctorArgs)
                    (_constrAppParameters ^.. each . Internal.patternArgPattern)
                )
        fmap (isCtor :) subConditions
      Internal.PatternVariable {} -> return []

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
      (decls :: [Function], clauseResult) <- runOutputList (runReader bindings (goExpression (clause ^. Internal.clauseBody)))
      return (StatementReturn (Just (castClause clauseResult)), decls)
      where
        castClause clauseResult =
          castToType (CDeclType {_typeDeclType = funSig ^. funcReturnType, _typeIsPtr = funSig ^. funcIsPtr}) clauseResult

goExpression :: Members '[Reader Internal.InfoTable, Reader Typed.TypesTable, Builtins, Reader PatternInfoTable] r => Internal.Expression -> Sem r Expression
goExpression = \case
  Internal.ExpressionIden i -> do
    let rootFunInternalName = Internal.getName i
        rootFunName = mkName rootFunInternalName
        evalFunName = asEval (rootFunName <> "_0")
    case i of
      Internal.IdenVar {} -> goIden i
      _ -> do
        (t, _) <- getType i
        let argTyps = t ^. cFunArgTypes
        (if null argTyps then goIden i else return $ functionCall (ExpressionVar evalFunName) [])
  Internal.ExpressionApplication a -> goApplication a
  Internal.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))
  Internal.ExpressionFunction {} -> impossible
  Internal.ExpressionHole {} -> impossible
  Internal.ExpressionUniverse {} -> impossible
  Internal.ExpressionSimpleLambda {} -> impossible
  Internal.ExpressionLambda {} -> impossible
  Internal.ExpressionLet {} -> impossible

goIden :: Members '[Reader PatternInfoTable, Builtins, Reader Internal.InfoTable] r => Internal.Iden -> Sem r Expression
goIden = \case
  Internal.IdenFunction n -> do
    funInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoFunctions)
    let varName = case funInfo ^. Internal.functionInfoDef . Internal.funDefBuiltin of
          Just builtin -> fromJust (builtinFunctionName builtin)
          Nothing -> mkName n
    return (ExpressionVar varName)
  Internal.IdenConstructor n -> ExpressionVar <$> getConstructorCName n
  Internal.IdenVar n ->
    (^. bindingInfoExpr) . HashMap.lookupDefault impossible (n ^. Internal.nameText) <$> asks (^. patternBindings)
  Internal.IdenAxiom n -> ExpressionVar <$> getAxiomCName n
  Internal.IdenInductive {} -> impossible

goApplication :: forall r. Members '[Reader PatternInfoTable, Reader Typed.TypesTable, Builtins, Reader Internal.InfoTable] r => Internal.Application -> Sem r Expression
goApplication a = do
  (f, args0) <- Trans.unfoldPolyApplication a
  if
      | null args0 -> goExpression f
      | otherwise -> case f of
          Internal.ExpressionLiteral {} -> goExpression f
          Internal.ExpressionIden iden -> do
            fArgs <- toList <$> mapM goExpression args0
            case iden of
              Internal.IdenVar n -> do
                BindingInfo {..} <- HashMap.lookupDefault impossible (n ^. Internal.nameText) <$> asks (^. patternBindings)
                return $ juvixFunctionCall _bindingInfoType _bindingInfoExpr fArgs
              Internal.IdenFunction n -> do
                info <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoFunctions)
                nPatterns <- functionInfoPatternsNum info
                (idenType, _) <- getType iden
                let nArgTyps = length (idenType ^. cFunArgTypes)
                if
                    | length fArgs < nArgTyps -> do
                        let name = mkName (Internal.getName iden)
                            evalName = asEval (name <> "_" <> show (length fArgs))
                        return $ functionCallCasted idenType (ExpressionVar evalName) fArgs
                    | nPatterns < nArgTyps -> do
                        idenExp <- goIden iden
                        let callTyp = idenType {_cFunArgTypes = drop nPatterns (idenType ^. cFunArgTypes)}
                            patternArgs = take nPatterns fArgs
                            funCall =
                              (if null patternArgs then idenExp else functionCallCasted idenType idenExp patternArgs)
                        return $ juvixFunctionCall callTyp funCall (drop nPatterns fArgs)
                    | otherwise -> do
                        idenExp <- goIden iden
                        return $ functionCallCasted idenType idenExp fArgs
              Internal.IdenConstructor n -> returnFunCall iden fArgs n
              Internal.IdenAxiom n -> returnFunCall iden fArgs n
              Internal.IdenInductive {} -> impossible
          _ -> impossible
  where
    returnFunCall :: Internal.Iden -> [Expression] -> Internal.Name -> Sem r Expression
    returnFunCall iden fArgs name = do
      (idenType, _) <- getType iden
      ( if length fArgs < length (idenType ^. cFunArgTypes)
          then
            ( do
                let evalName = asEval (mkName name <> "_" <> show (length fArgs))
                return $ functionCallCasted idenType (ExpressionVar evalName) fArgs
            )
          else
            ( do
                idenExp <- goIden iden
                return $ functionCallCasted idenType idenExp fArgs
            )
        )

goLiteral :: C.LiteralLoc -> Literal
goLiteral l = case l ^. C.withLocParam of
  C.LitString s -> LiteralString s
  C.LitInteger i -> LiteralInt i

goAxiom ::
  Members [Reader Internal.InfoTable, Reader CompileInfoTable] r =>
  Internal.AxiomDef ->
  Sem r [CCode]
goAxiom a
  | isJust (a ^. Internal.axiomBuiltin) = return []
  | otherwise = do
      backend <- runFail (lookupBackends (axiomName ^. Internal.nameId) >>= firstBackendMatch)
      case backend of
        Nothing -> genFunctionDef a
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
    axiomName :: Internal.Name
    axiomName = a ^. Internal.axiomName
    defineName :: Text
    defineName = mkName axiomName
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendC == b ^. backendItemBackend . withLocParam)
        $> b
          ^. backendItemCode
    firstBackendMatch ::
      Member Fail r =>
      [BackendItem] ->
      Sem r Text
    firstBackendMatch = failMaybe . firstJust getCode
    lookupBackends ::
      Members '[Fail, Reader CompileInfoTable] r =>
      NameId ->
      Sem r [BackendItem]
    lookupBackends f = ask >>= failMaybe . fmap (^. Scoper.compileInfoBackendItems) . HashMap.lookup f
    genFunctionDef ::
      forall r.
      Members [Reader Internal.InfoTable, Reader CompileInfoTable] r =>
      Internal.AxiomDef ->
      Sem r [CCode]
    genFunctionDef d
      | validWasmIdent nameText = (ExternalAttribute (ImportName (axiomName ^. Internal.nameText)) :) <$> sig
      | otherwise = sig
      where
        nameText :: Text
        nameText = axiomName ^. Internal.nameText

        sig :: Sem r [CCode]
        sig = do
          s <- cFunTypeToFunSig defineName <$> typeToFunType (mkPolyType' (d ^. Internal.axiomType))
          return [ExternalFuncSig s]

goForeign :: ForeignBlock -> [CCode]
goForeign b = case b ^. foreignBackend . withLocParam of
  BackendC -> [Verbatim (b ^. foreignCode)]
  _ -> []

mkInductiveName :: Internal.InductiveDef -> Text
mkInductiveName i = mkName (i ^. Internal.inductiveName)

mkInductiveConstructorNames :: Internal.InductiveDef -> [Text]
mkInductiveConstructorNames i = mkName . view Internal.inductiveConstructorName <$> i ^. Internal.inductiveConstructors

mkInductiveTypeDef :: Internal.InductiveDef -> [CCode]
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
    baseName = mkName (i ^. Internal.inductiveName)

goInductiveDef :: Members '[Reader Internal.InfoTable] r => Internal.InductiveDef -> Sem r [CCode]
goInductiveDef i
  | isJust (i ^. Internal.inductiveBuiltin) = return []
  | otherwise = do
      ctorDefs <- concatMapM goInductiveConstructorDef (i ^. Internal.inductiveConstructors)
      ctorNews <- concatMapM (goInductiveConstructorNew i) (i ^. Internal.inductiveConstructors)
      projections <- concatMapM (goProjections inductiveTypeDef) (i ^. Internal.inductiveConstructors)
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
    baseName = mkName (i ^. Internal.inductiveName)

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
  Members '[Reader Internal.InfoTable] r =>
  Internal.InductiveDef ->
  Internal.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorNew i ctor = ctorNewFun
  where
    ctorNewFun :: Sem r [CCode]
    ctorNewFun = if null ctorParams then return ctorNewNullary else ctorNewNary

    baseName :: Text
    baseName = mkName (ctor ^. Internal.inductiveConstructorName)

    inductiveName :: Text
    inductiveName = mkInductiveName i

    ctorParams :: [Internal.PolyType]
    ctorParams = map mkPolyType' (ctor ^. Internal.inductiveConstructorParameters)

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

inductiveCtorParams :: Members '[Reader Internal.InfoTable] r => Internal.InductiveConstructorDef -> Sem r [CDeclType]
inductiveCtorParams ctor = mapM (goType . mkPolyType') (ctor ^. Internal.inductiveConstructorParameters)

inductiveCtorArgs :: Members '[Reader Internal.InfoTable] r => Internal.InductiveConstructorDef -> Sem r [Declaration]
inductiveCtorArgs ctor = namedArgs asCtorArg <$> inductiveCtorParams ctor

inductiveCtorTypes :: Members '[Reader Internal.InfoTable] r => Internal.Name -> Sem r [CDeclType]
inductiveCtorTypes ctor = do
  info <- Internal.lookupConstructor ctor
  mapM (goType . mkPolyType') (snd (Internal.constructorArgTypes info))

goInductiveConstructorDef ::
  forall r.
  Members '[Reader Internal.InfoTable] r =>
  Internal.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorDef ctor = do
  d <- ctorDecl
  return [ExternalDecl d]
  where
    ctorDecl :: Sem r Declaration
    ctorDecl = if null ctorParams then return ctorBool else ctorStruct

    baseName :: Text
    baseName = mkName (ctor ^. Internal.inductiveConstructorName)

    ctorParams :: [Internal.PolyType]
    ctorParams = map mkPolyType' (ctor ^. Internal.inductiveConstructorParameters)

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
  Members '[Reader Internal.InfoTable] r =>
  DeclType ->
  Internal.InductiveConstructorDef ->
  Sem r [CCode]
goProjections inductiveTypeDef ctor = do
  params <- inductiveCtorParams ctor
  return (ExternalFunc <$> zipWith projFunction [0 ..] params)
  where
    baseName :: Text
    baseName = mkName (ctor ^. Internal.inductiveConstructorName)

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
