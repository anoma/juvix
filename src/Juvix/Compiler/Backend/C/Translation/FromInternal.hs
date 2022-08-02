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
import Juvix.Compiler.Internal.Extra (mkPolyType')
import Juvix.Compiler.Internal.Extra qualified as Micro
import Juvix.Compiler.Internal.Translation.FromAbstract qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as Micro1
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Micro
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

type CompileInfoTable = HashMap Scoper.NameId Scoper.CompileInfo

compileInfoTable :: Micro.InternalTypedResult -> CompileInfoTable
compileInfoTable r =
  HashMap.mapKeys
    (^. Scoper.nameId)
    ( r
        ^. Micro.resultInternalArityResult
        . Micro1.resultInternalResult
        . Internal.resultAbstract
        . Abstract.resultScoper
        . Scoper.resultScoperTable
        . Scoper.infoCompilationRules
    )

fromInternal ::
  forall r.
  Member Builtins r =>
  Micro.InternalTypedResult ->
  Sem r MiniCResult
fromInternal i = MiniCResult . serialize <$> cunitResult
  where
    compileInfo :: CompileInfoTable
    compileInfo = compileInfoTable i

    typesTable :: Micro.TypesTable
    typesTable = i ^. Micro.resultIdenTypes

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

    cmodule :: Micro.Module -> Sem r [CCode]
    cmodule m = do
      let buildTable = Micro.buildTable [m]
      let defs =
            genStructDefs m
              <> run (runReader compileInfo (runReader buildTable (genAxioms m)))
              <> run (runReader buildTable (genCTypes m))
              <> run (runReader buildTable (runReader typesTable (genFunctionSigs m)))
              <> run (runReader buildTable (runReader typesTable (genClosures m)))
      funDefs <- runReader buildTable (runReader typesTable (genFunctionDefs m))
      return (defs <> funDefs)

    cmodules :: Sem r [CCode]
    cmodules = concatMapM cmodule (toList (i ^. Micro.resultModules))

genStructDefs :: Micro.Module -> [CCode]
genStructDefs Micro.Module {..} =
  concatMap go (_moduleBody ^. Micro.moduleStatements)
  where
    go :: Micro.Statement -> [CCode]
    go = \case
      Micro.StatementInductive d -> mkInductiveTypeDef d
      Micro.StatementInclude i ->
        concatMap go (i ^. Micro.includeModule . Micro.moduleBody . Micro.moduleStatements)
      _ -> []

genAxioms :: forall r. Members '[Reader Micro.InfoTable, Reader CompileInfoTable] r => Micro.Module -> Sem r [CCode]
genAxioms Micro.Module {..} =
  concatMapM go (_moduleBody ^. Micro.moduleStatements)
  where
    go :: Micro.Statement -> Sem r [CCode]
    go = \case
      Micro.StatementInductive {} -> return []
      Micro.StatementAxiom d -> goAxiom d
      Micro.StatementForeign {} -> return []
      Micro.StatementFunction {} -> return []
      Micro.StatementInclude i ->
        concatMapM go (i ^. Micro.includeModule . Micro.moduleBody . Micro.moduleStatements)

genCTypes :: forall r. Member (Reader Micro.InfoTable) r => Micro.Module -> Sem r [CCode]
genCTypes Micro.Module {..} =
  concatMapM go (_moduleBody ^. Micro.moduleStatements)
  where
    go :: Micro.Statement -> Sem r [CCode]
    go = \case
      Micro.StatementInductive d -> goInductiveDef d
      Micro.StatementAxiom {} -> return []
      Micro.StatementForeign d -> return (goForeign d)
      Micro.StatementFunction {} -> return []
      Micro.StatementInclude i ->
        concatMapM go (i ^. Micro.includeModule . Micro.moduleBody . Micro.moduleStatements)

genFunctionSigs :: forall r. Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r => Micro.Module -> Sem r [CCode]
genFunctionSigs Micro.Module {..} =
  concatMapM (applyOnFunStatement genFunctionDef) (_moduleBody ^. Micro.moduleStatements)
  where
    genFunctionDef :: Micro.FunctionDef -> Sem r [CCode]
    genFunctionDef d
      | T.all isAlphaNum nameText = (ExternalAttribute (ExportName nameText) :) <$> sig
      | otherwise = sig
      where
        nameText :: Text
        nameText = d ^. Micro.funDefName . Micro.nameText

        sig :: Sem r [CCode]
        sig = genFunctionSig d

genFunctionDefs ::
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Builtins] r =>
  Micro.Module ->
  Sem r [CCode]
genFunctionDefs Micro.Module {..} = genFunctionDefsBody _moduleBody

genFunctionDefsBody ::
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Builtins] r =>
  Micro.ModuleBody ->
  Sem r [CCode]
genFunctionDefsBody Micro.ModuleBody {..} =
  concatMapM (applyOnFunStatement goFunctionDef) _moduleStatements

isNullary :: Text -> CFunType -> Bool
isNullary funName funType = null (funType ^. cFunArgTypes) && funName /= Str.main_

mkFunctionSig :: forall r. Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r => Micro.FunctionDef -> Sem r FunctionSig
mkFunctionSig Micro.FunctionDef {..} =
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

genFunctionSig :: forall r. Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r => Micro.FunctionDef -> Sem r [CCode]
genFunctionSig d@(Micro.FunctionDef {..}) = do
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
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Builtins] r =>
  Micro.FunctionDef ->
  Sem r [CCode]
goFunctionDef d@(Micro.FunctionDef {..})
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
                            ^. Micro.nameText
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
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Builtins] r =>
  FunctionSig ->
  [Micro.PolyType] ->
  Micro.FunctionClause ->
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

    patternCondition :: Expression -> Micro.Pattern -> Sem r [Expression]
    patternCondition arg = \case
      Micro.PatternConstructorApp Micro.ConstructorApp {..} -> do
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
                    (_constrAppParameters ^.. each . Micro.patternArgPattern)
                )
        fmap (isCtor :) subConditions
      Micro.PatternVariable {} -> return []
      Micro.PatternWildcard {} -> return []

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
      (decls :: [Function], clauseResult) <- runOutputList (runReader bindings (goExpression (clause ^. Micro.clauseBody)))
      return (StatementReturn (Just (castClause clauseResult)), decls)
      where
        castClause clauseResult =
          castToType (CDeclType {_typeDeclType = funSig ^. funcReturnType, _typeIsPtr = funSig ^. funcIsPtr}) clauseResult

goExpression :: Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Builtins, Reader PatternInfoTable] r => Micro.Expression -> Sem r Expression
goExpression = \case
  Micro.ExpressionIden i -> do
    let rootFunMicroName = Micro.getName i
        rootFunName = mkName rootFunMicroName
        evalFunName = asEval (rootFunName <> "_0")
    case i of
      Micro.IdenVar {} -> goIden i
      _ -> do
        (t, _) <- getType i
        let argTyps = t ^. cFunArgTypes
        (if null argTyps then goIden i else return $ functionCall (ExpressionVar evalFunName) [])
  Micro.ExpressionApplication a -> goApplication a
  Micro.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))
  Micro.ExpressionFunction {} -> impossible
  Micro.ExpressionHole {} -> impossible
  Micro.ExpressionUniverse {} -> impossible
  Micro.ExpressionLambda {} -> impossible

goIden :: Members '[Reader PatternInfoTable, Builtins, Reader Micro.InfoTable] r => Micro.Iden -> Sem r Expression
goIden = \case
  Micro.IdenFunction n -> do
    funInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoFunctions)
    let varName = case funInfo ^. Micro.functionInfoDef . Micro.funDefBuiltin of
          Just builtin -> fromJust (builtinFunctionName builtin)
          Nothing -> mkName n
    return (ExpressionVar varName)
  Micro.IdenConstructor n -> ExpressionVar <$> getConstructorCName n
  Micro.IdenVar n ->
    (^. bindingInfoExpr) . HashMap.lookupDefault impossible (n ^. Micro.nameText) <$> asks (^. patternBindings)
  Micro.IdenAxiom n -> ExpressionVar <$> getAxiomCName n
  Micro.IdenInductive {} -> impossible

goApplication :: forall r. Members '[Reader PatternInfoTable, Reader Micro.TypesTable, Builtins, Reader Micro.InfoTable] r => Micro.Application -> Sem r Expression
goApplication a = do
  (f, args0) <- unfoldPolyApp a
  if
      | null args0 -> goExpression f
      | otherwise -> case f of
          Micro.ExpressionLiteral {} -> goExpression f
          Micro.ExpressionIden iden -> do
            fArgs <- toList <$> mapM goExpression args0
            case iden of
              Micro.IdenVar n -> do
                BindingInfo {..} <- HashMap.lookupDefault impossible (n ^. Micro.nameText) <$> asks (^. patternBindings)
                return $ juvixFunctionCall _bindingInfoType _bindingInfoExpr fArgs
              Micro.IdenFunction n -> do
                info <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoFunctions)
                nPatterns <- functionInfoPatternsNum info
                (idenType, _) <- getType iden
                let nArgTyps = length (idenType ^. cFunArgTypes)
                if
                    | length fArgs < nArgTyps -> do
                        let name = mkName (Micro.getName iden)
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
              Micro.IdenConstructor n -> returnFunCall iden fArgs n
              Micro.IdenAxiom n -> returnFunCall iden fArgs n
              Micro.IdenInductive {} -> impossible
          _ -> impossible
  where
    returnFunCall :: Micro.Iden -> [Expression] -> Micro.Name -> Sem r Expression
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
  Members [Reader Micro.InfoTable, Reader CompileInfoTable] r =>
  Micro.AxiomDef ->
  Sem r [CCode]
goAxiom a
  | isJust (a ^. Micro.axiomBuiltin) = return []
  | otherwise = do
      backend <- runFail (lookupBackends (axiomName ^. Micro.nameId) >>= firstBackendMatch)
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
    axiomName :: Micro.Name
    axiomName = a ^. Micro.axiomName
    defineName :: Text
    defineName = mkName axiomName
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendC == b ^. backendItemBackend)
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
      Members [Reader Micro.InfoTable, Reader CompileInfoTable] r =>
      Micro.AxiomDef ->
      Sem r [CCode]
    genFunctionDef d
      | T.all isAlphaNum nameText = (ExternalAttribute (ImportName (axiomName ^. Micro.nameText)) :) <$> sig
      | otherwise = sig
      where
        nameText :: Text
        nameText = axiomName ^. Micro.nameText

        sig :: Sem r [CCode]
        sig = do
          s <- cFunTypeToFunSig defineName <$> typeToFunType (mkPolyType' (d ^. Micro.axiomType))
          return [ExternalFuncSig s]

goForeign :: ForeignBlock -> [CCode]
goForeign b = case b ^. foreignBackend of
  BackendC -> [Verbatim (b ^. foreignCode)]
  _ -> []

mkInductiveName :: Micro.InductiveDef -> Text
mkInductiveName i = mkName (i ^. Micro.inductiveName)

mkInductiveConstructorNames :: Micro.InductiveDef -> [Text]
mkInductiveConstructorNames i = mkName . view Micro.inductiveConstructorName <$> i ^. Micro.inductiveConstructors

mkInductiveTypeDef :: Micro.InductiveDef -> [CCode]
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
    baseName = mkName (i ^. Micro.inductiveName)

goInductiveDef :: Members '[Reader Micro.InfoTable] r => Micro.InductiveDef -> Sem r [CCode]
goInductiveDef i
  | isJust (i ^. Micro.inductiveBuiltin) = return []
  | otherwise = do
      ctorDefs <- concatMapM goInductiveConstructorDef (i ^. Micro.inductiveConstructors)
      ctorNews <- concatMapM (goInductiveConstructorNew i) (i ^. Micro.inductiveConstructors)
      projections <- concatMapM (goProjections inductiveTypeDef) (i ^. Micro.inductiveConstructors)
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
    baseName = mkName (i ^. Micro.inductiveName)

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
  Members '[Reader Micro.InfoTable] r =>
  Micro.InductiveDef ->
  Micro.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorNew i ctor = ctorNewFun
  where
    ctorNewFun :: Sem r [CCode]
    ctorNewFun = if null ctorParams then return ctorNewNullary else ctorNewNary

    baseName :: Text
    baseName = mkName (ctor ^. Micro.inductiveConstructorName)

    inductiveName :: Text
    inductiveName = mkInductiveName i

    ctorParams :: [Micro.PolyType]
    ctorParams = map mkPolyType' (ctor ^. Micro.inductiveConstructorParameters)

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

inductiveCtorParams :: Members '[Reader Micro.InfoTable] r => Micro.InductiveConstructorDef -> Sem r [CDeclType]
inductiveCtorParams ctor = mapM (goType . mkPolyType') (ctor ^. Micro.inductiveConstructorParameters)

inductiveCtorArgs :: Members '[Reader Micro.InfoTable] r => Micro.InductiveConstructorDef -> Sem r [Declaration]
inductiveCtorArgs ctor = namedArgs asCtorArg <$> inductiveCtorParams ctor

inductiveCtorTypes :: Members '[Reader Micro.InfoTable] r => Micro.Name -> Sem r [CDeclType]
inductiveCtorTypes ctor = do
  info <- Micro.lookupConstructor ctor
  mapM (goType . mkPolyType') (snd (Micro.constructorArgTypes info))

goInductiveConstructorDef ::
  forall r.
  Members '[Reader Micro.InfoTable] r =>
  Micro.InductiveConstructorDef ->
  Sem r [CCode]
goInductiveConstructorDef ctor = do
  d <- ctorDecl
  return [ExternalDecl d]
  where
    ctorDecl :: Sem r Declaration
    ctorDecl = if null ctorParams then return ctorBool else ctorStruct

    baseName :: Text
    baseName = mkName (ctor ^. Micro.inductiveConstructorName)

    ctorParams :: [Micro.PolyType]
    ctorParams = map mkPolyType' (ctor ^. Micro.inductiveConstructorParameters)

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
  Members '[Reader Micro.InfoTable] r =>
  DeclType ->
  Micro.InductiveConstructorDef ->
  Sem r [CCode]
goProjections inductiveTypeDef ctor = do
  params <- inductiveCtorParams ctor
  return (ExternalFunc <$> zipWith projFunction [0 ..] params)
  where
    baseName :: Text
    baseName = mkName (ctor ^. Micro.inductiveConstructorName)

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
