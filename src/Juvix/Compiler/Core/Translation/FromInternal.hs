module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Eta (etaExpandApps)
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Data.Loc qualified as Loc
import Juvix.Extra.Strings qualified as Str

unsupported :: Text -> a
unsupported thing = error ("Internal to Core: Not yet supported: " <> thing)

isExplicit :: Internal.PatternArg -> Bool
isExplicit = (== Internal.Explicit) . (^. Internal.patternArgIsImplicit)

-- Translation of a Name into the identifier index used in the Core InfoTable
mkIdentIndex :: Name -> Text
mkIdentIndex = show . (^. Internal.nameId . Internal.unNameId)

fromInternal :: Internal.InternalTypedResult -> Sem k CoreResult
fromInternal i = do
  (res, _) <- runInfoTableBuilder emptyInfoTable (runReader (i ^. InternalTyped.resultIdenTypes) f)
  return $
    CoreResult
      { _coreResultTable = res,
        _coreResultInternalTypedResult = i
      }
  where
    f :: Members '[InfoTableBuilder, Reader InternalTyped.TypesTable] r => Sem r ()
    f = do
      let resultModules = toList (i ^. InternalTyped.resultModules)
      runNameIdGen (runReader (Internal.buildTable resultModules) (mapM_ coreModule resultModules))
      where
        coreModule :: Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, NameIdGen] r => Internal.Module -> Sem r ()
        coreModule m = do
          registerInductiveDefs m
          registerFunctionDefs m

fromInternalExpression :: CoreResult -> Internal.Expression -> Sem r Node
fromInternalExpression res exp = do
  let modules = res ^. coreResultInternalTypedResult . InternalTyped.resultModules
  snd
    <$> runReader
      (Internal.buildTable modules)
      ( runInfoTableBuilder
          (res ^. coreResultTable)
          ( runReader
              (res ^. coreResultInternalTypedResult . InternalTyped.resultIdenTypes)
              (runReader initIndexTable (goExpression exp))
          )
      )

registerInductiveDefs ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.Module ->
  Sem r ()
registerInductiveDefs m = registerInductiveDefsBody (m ^. Internal.moduleBody)

registerInductiveDefsBody ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.ModuleBody ->
  Sem r ()
registerInductiveDefsBody body = mapM_ go (body ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementInductive d -> goInductiveDef d
      Internal.StatementAxiom {} -> return ()
      Internal.StatementForeign {} -> return ()
      Internal.StatementFunction {} -> return ()
      Internal.StatementInclude i ->
        mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

registerFunctionDefs ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, NameIdGen, Reader Internal.InfoTable] r =>
  Internal.Module ->
  Sem r ()
registerFunctionDefs m = registerFunctionDefsBody (m ^. Internal.moduleBody)

registerFunctionDefsBody ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, NameIdGen, Reader Internal.InfoTable] r =>
  Internal.ModuleBody ->
  Sem r ()
registerFunctionDefsBody body = mapM_ go (body ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementFunction f -> goMutualBlock f
      Internal.StatementAxiom a -> goAxiomInductive a >> goAxiomDef a
      Internal.StatementInclude i -> mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)
      _ -> return ()

goInductiveDef ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.InductiveDef ->
  Sem r ()
goInductiveDef i = do
  sym <- freshSymbol
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  let info =
        InductiveInfo
          { _inductiveName = i ^. Internal.inductiveName . nameText,
            _inductiveLocation = Just $ i ^. Internal.inductiveName . nameLoc,
            _inductiveSymbol = sym,
            _inductiveKind = mkDynamic',
            _inductiveConstructors = ctorInfos,
            _inductiveParams = [],
            _inductivePositive = i ^. Internal.inductivePositive,
            _inductiveBuiltin = i ^. Internal.inductiveBuiltin
          }
  registerInductive (mkIdentIndex (i ^. Internal.inductiveName)) info

goConstructor ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable] r =>
  Symbol ->
  Internal.InductiveConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  mblt <- mBuiltin
  tag <- ctorTag mblt
  ty <- ctorType
  let info =
        ConstructorInfo
          { _constructorName = ctor ^. Internal.inductiveConstructorName . nameText,
            _constructorLocation = Just $ ctor ^. Internal.inductiveConstructorName . nameLoc,
            _constructorTag = tag,
            _constructorType = ty,
            _constructorArgsNum = length (ctor ^. Internal.inductiveConstructorParameters),
            _constructorInductive = sym,
            _constructorBuiltin = mblt
          }
  registerConstructor (mkIdentIndex (ctor ^. Internal.inductiveConstructorName)) info
  return info
  where
    mBuiltin :: Sem r (Maybe Internal.BuiltinConstructor)
    mBuiltin =
      (^. Internal.constructorInfoBuiltin)
        . HashMap.lookupDefault impossible (ctor ^. Internal.inductiveConstructorName)
        <$> asks (^. Internal.infoConstructors)

    ctorTag :: Maybe Internal.BuiltinConstructor -> Sem r Tag
    ctorTag = \case
      Just Internal.BuiltinBoolTrue -> return (BuiltinTag TagTrue)
      Just Internal.BuiltinBoolFalse -> return (BuiltinTag TagFalse)
      Just Internal.BuiltinNatZero -> freshTag
      Just Internal.BuiltinNatSuc -> freshTag
      Nothing -> freshTag

    ctorType :: Sem r Type
    ctorType =
      runReader
        initIndexTable
        ( Internal.constructorType (ctor ^. Internal.inductiveConstructorName)
            >>= goExpression
        )

goMutualBlock ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, NameIdGen, Reader Internal.InfoTable] r =>
  Internal.MutualBlock ->
  Sem r ()
goMutualBlock m = do
  funcsWithSym <- mapM withSym (m ^. Internal.mutualFunctions)
  mapM_ goFunctionDefIden funcsWithSym
  mapM_ goFunctionDef funcsWithSym
  where
    withSym :: a -> Sem r (a, Symbol)
    withSym x = do
      sym <- freshSymbol
      return (x, sym)

goFunctionDefIden ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, NameIdGen] r =>
  (Internal.FunctionDef, Symbol) ->
  Sem r ()
goFunctionDefIden (f, sym) = do
  funTy <- runReader initIndexTable (goExpression (f ^. Internal.funDefType))
  let info =
        IdentifierInfo
          { _identifierName = f ^. Internal.funDefName . nameText,
            _identifierLocation = Just $ f ^. Internal.funDefName . nameLoc,
            _identifierSymbol = sym,
            _identifierType = funTy,
            _identifierArgsNum = 0,
            _identifierArgsInfo = [],
            _identifierIsExported = False,
            _identifierBuiltin = f ^. Internal.funDefBuiltin
          }
  registerIdent (mkIdentIndex (f ^. Internal.funDefName)) info
  when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)

goFunctionDef ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  (Internal.FunctionDef, Symbol) ->
  Sem r ()
goFunctionDef (f, sym) = do
  mbody <- case f ^. Internal.funDefBuiltin of
    Just Internal.BuiltinBoolIf -> return Nothing
    Just Internal.BuiltinNatPlus -> Just <$> mkBody
    Nothing -> Just <$> mkBody
  forM_ mbody (registerIdentNode sym)
  where
    mkBody :: Sem r Node
    mkBody =
      if
          | nExplicitPatterns == 0 -> runReader initIndexTable (goExpression (f ^. Internal.funDefClauses . _head1 . Internal.clauseBody))
          | otherwise ->
              ( do
                  let values :: [Node]
                      values = mkVar Info.empty <$> vs
                      indexTable :: IndexTable
                      indexTable = IndexTable {_indexTableVarsNum = nExplicitPatterns, _indexTableVars = mempty}
                  ms <- mapM (runReader indexTable . goFunctionClause) (f ^. Internal.funDefClauses)
                  let match = mkMatch' (fromList values) (toList ms)
                  return $ foldr (\_ n -> mkLambda' n) match vs
              )
    -- Assumption: All clauses have the same number of patterns
    nExplicitPatterns :: Int
    nExplicitPatterns = length $ filter isExplicit (f ^. Internal.funDefClauses . _head1 . Internal.clausePatterns)

    vs :: [Index]
    vs = take nExplicitPatterns [0 ..]

goLambda ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.Lambda ->
  Sem r Node
goLambda l = do
  ms <-
    local
      (over indexTableVarsNum (+ nPatterns))
      (mapM goLambdaClause (l ^. Internal.lambdaClauses))
  values' <- values
  let match = mkMatch' (fromList values') (toList ms)
  return $ foldr (\_ n -> mkLambda' n) match values'
  where
    nPatterns :: Int
    nPatterns = length (l ^. Internal.lambdaClauses . _head1 . Internal.lambdaPatterns)

    values :: Sem r [Node]
    values = do
      varsNum <- asks (^. indexTableVarsNum)
      let vs = take nPatterns [varsNum ..]
      return (mkVar' <$> vs)

goAxiomInductive ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomInductive a = whenJust (a ^. Internal.axiomBuiltin) builtinInductive
  where
    builtinInductive :: Internal.BuiltinAxiom -> Sem r ()
    builtinInductive = \case
      Internal.BuiltinNatPrint -> return ()
      Internal.BuiltinStringPrint -> return ()
      Internal.BuiltinBoolPrint -> return ()
      Internal.BuiltinIOSequence -> return ()
      Internal.BuiltinString -> registerInductiveAxiom
      Internal.BuiltinIO -> registerInductiveAxiom

    registerInductiveAxiom :: Sem r ()
    registerInductiveAxiom = do
      sym <- freshSymbol
      let info =
            InductiveInfo
              { _inductiveName = a ^. Internal.axiomName . nameText,
                _inductiveLocation = Just $ a ^. Internal.axiomName . nameLoc,
                _inductiveSymbol = sym,
                _inductiveKind = mkDynamic',
                _inductiveConstructors = [],
                _inductiveParams = [],
                _inductivePositive = False,
                _inductiveBuiltin = Nothing
              }
      registerInductive (mkIdentIndex (a ^. Internal.axiomName)) info

goAxiomDef ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomDef a = case a ^. Internal.axiomBuiltin >>= builtinBody of
  Just body -> do
    sym <- freshSymbol
    ty <- axiomType'
    let info =
          IdentifierInfo
            { _identifierName = a ^. Internal.axiomName . nameText,
              _identifierLocation = Just $ a ^. Internal.axiomName . nameLoc,
              _identifierSymbol = sym,
              _identifierType = ty,
              _identifierArgsNum = 0,
              _identifierArgsInfo = [],
              _identifierIsExported = False,
              _identifierBuiltin = Nothing
            }
    registerIdent (mkIdentIndex (a ^. Internal.axiomName)) info
    registerIdentNode sym body
  Nothing -> return ()
  where
    builtinBody :: Internal.BuiltinAxiom -> Maybe Node
    builtinBody = \case
      Internal.BuiltinNatPrint -> Just writeLambda
      Internal.BuiltinStringPrint -> Just writeLambda
      Internal.BuiltinBoolPrint -> Just writeLambda
      Internal.BuiltinIOSequence ->
        Just
          ( mkLambda'
              ( mkLambda'
                  ( mkConstr'
                      (BuiltinTag TagBind)
                      [mkVar' 1, mkLambda' (mkVar' 1)]
                  )
              )
          )
      Internal.BuiltinString -> Nothing
      Internal.BuiltinIO -> Nothing

    axiomType' :: Sem r Type
    axiomType' = runReader initIndexTable (goExpression (a ^. Internal.axiomType))

    writeLambda :: Node
    writeLambda = mkLambda' (mkConstr' (BuiltinTag TagWrite) [mkVar' 0])

fromPattern ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
  Internal.Pattern ->
  Sem r Pattern
fromPattern = \case
  Internal.PatternWildcard {} -> return wildcard
  Internal.PatternVariable n -> return $ PatBinder (PatternBinder (Binder (n ^. nameText) (Just (n ^. nameLoc)) mkDynamic') wildcard)
  Internal.PatternConstructorApp c -> do
    let n = c ^. Internal.constrAppConstructor
        explicitPatterns =
          (^. Internal.patternArgPattern)
            <$> filter
              isExplicit
              (c ^. Internal.constrAppParameters)
    args <- mapM fromPattern explicitPatterns
    m <- getIdent identIndex
    case m of
      Just (IdentConstr tag) -> return $ PatConstr (PatternConstr (setInfoLocation (n ^. nameLoc) (setInfoName (n ^. nameText) Info.empty)) tag args)
      Just _ -> error ("internal to core: not a constructor " <> txt)
      Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    where
      identIndex :: Text
      identIndex = mkIdentIndex (c ^. Internal.constrAppConstructor)

      txt :: Text
      txt = c ^. Internal.constrAppConstructor . Internal.nameText
  where
    wildcard :: Pattern
    wildcard = PatWildcard (PatternWildcard Info.empty)

getPatternVars :: Internal.Pattern -> [Name]
getPatternVars = \case
  Internal.PatternWildcard {} -> []
  Internal.PatternVariable n -> [n]
  Internal.PatternConstructorApp c ->
    concatMap getPatternVars explicitPatterns
    where
      explicitPatterns =
        (^. Internal.patternArgPattern)
          <$> filter
            isExplicit
            (c ^. Internal.constrAppParameters)

goPatterns ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.Expression ->
  [Internal.Pattern] ->
  Sem r MatchBranch
goPatterns body ps = do
  vars <- asks (^. indexTableVars)
  varsNum <- asks (^. indexTableVarsNum)
  pats <- patterns
  let bs :: [Name]
      bs = concatMap getPatternVars (reverse ps)
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameId) k vs, k + 1)
          )
          (vars, varsNum)
          bs
      body' :: Sem r Node
      body' =
        local
          (set indexTableVars vars' . set indexTableVarsNum varsNum')
          (goExpression body)
  MatchBranch Info.empty (fromList pats) <$> body'
  where
    patterns :: Sem r [Pattern]
    patterns = reverse <$> mapM fromPattern ps

goFunctionClause ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.FunctionClause ->
  Sem r MatchBranch
goFunctionClause clause =
  local
    (over indexTableVars (HashMap.union patternArgs))
    (goPatterns (clause ^. Internal.clauseBody) ps)
  where
    explicitPatternArgs :: [Internal.PatternArg]
    explicitPatternArgs = filter isExplicit (clause ^. Internal.clausePatterns)

    ps :: [Internal.Pattern]
    ps = (^. Internal.patternArgPattern) <$> explicitPatternArgs

    patternArgs :: HashMap NameId Index
    patternArgs = HashMap.fromList (first (^. nameId) <$> patternArgNames)
      where
        patternArgNames :: [(Name, Index)]
        patternArgNames = catFstMaybes (first (^. Internal.patternArgName) <$> zip explicitPatternArgs [0 ..])

        catFstMaybes :: [(Maybe a, b)] -> [(a, b)]
        catFstMaybes = mapMaybe f
          where
            f :: (Maybe a, b) -> Maybe (a, b)
            f (x, y) = fmap (\x' -> (x', y)) x

goLambdaClause ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.LambdaClause ->
  Sem r MatchBranch
goLambdaClause clause = goPatterns (clause ^. Internal.lambdaBody) ps
  where
    ps :: [Internal.Pattern]
    ps = (^. Internal.patternArgPattern) <$> toList (clause ^. Internal.lambdaPatterns)

goExpression ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.Expression ->
  Sem r Node
goExpression e = do
  node <- goExpression' e
  tab <- getInfoTable
  return $ etaExpandApps tab node

goExpression' ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.Expression ->
  Sem r Node
goExpression' = \case
  Internal.ExpressionLiteral l -> return (goLiteral l)
  Internal.ExpressionIden i -> case i of
    Internal.IdenVar n -> do
      k <- HashMap.lookupDefault impossible id_ <$> asks (^. indexTableVars)
      varsNum <- asks (^. indexTableVarsNum)
      return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
    Internal.IdenFunction n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) sym
        Just _ -> error ("internal to core: not a function: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenInductive n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) sym []
        Just _ -> error ("internal to core: not an inductive: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenConstructor n -> do
      m <- getIdent identIndex
      case m of
        Just (IdentConstr tag) -> return (mkConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) tag [])
        Just _ -> error ("internal to core: not a constructor " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenAxiom n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) sym
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) sym []
        Just _ -> error ("internal to core: not an axiom: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    where
      identIndex :: Text
      identIndex = mkIdentIndex (Internal.getName i)

      id_ :: NameId
      id_ = Internal.getName i ^. nameId

      txt :: Text
      txt = Internal.getName i ^. Internal.nameText
  Internal.ExpressionApplication a -> goApplication a
  Internal.ExpressionSimpleLambda l -> goSimpleLambda l
  Internal.ExpressionLambda l -> goLambda l
  e@(Internal.ExpressionFunction {}) -> goFunction (Internal.unfoldFunType e)
  Internal.ExpressionHole h -> error ("goExpression hole: " <> show (Loc.getLoc h))
  Internal.ExpressionUniverse {} -> return (mkUniv' (fromIntegral smallLevel))

goFunction ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  ([Internal.FunctionParameter], Internal.Expression) ->
  Sem r Node
goFunction (params, returnTypeExpr) = foldr f (goExpression returnTypeExpr) params
  where
    f :: Internal.FunctionParameter -> Sem r Node -> Sem r Node
    f param acc = do
      paramBinder <- Binder (maybe "" (^. nameText) $ param ^. Internal.paramName) (fmap (^. nameLoc) $ param ^. Internal.paramName) <$> goExpression (param ^. Internal.paramType)
      case param ^. Internal.paramName of
        Nothing -> mkPi mempty paramBinder <$> acc
        Just vn -> mkPi mempty paramBinder <$> localAddName vn acc

goSimpleLambda ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.SimpleLambda ->
  Sem r Node
goSimpleLambda l = localAddName (l ^. Internal.slambdaVar) (mkLambda' <$> goExpression (l ^. Internal.slambdaBody))

goApplication ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable, Reader IndexTable] r =>
  Internal.Application ->
  Sem r Node
goApplication a = do
  (f, args) <- Internal.unfoldPolyApplication a
  let exprArgs :: Sem r [Node]
      exprArgs = mapM goExpression args

      app :: Sem r Node
      app = do
        fExpr <- goExpression f
        case a ^. Internal.appImplicit of
          Internal.Implicit -> return fExpr
          Internal.Explicit -> mkApps' fExpr <$> exprArgs

  case f of
    Internal.ExpressionIden (Internal.IdenFunction n) -> do
      funInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoFunctions)
      case funInfo ^. Internal.functionInfoDef . Internal.funDefBuiltin of
        Just Internal.BuiltinBoolIf -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (v : b1 : b2 : xs) -> return (mkApps' (mkIf' sym v b1 b2) xs)
            _ -> error "if must be called with 3 arguments"
        _ -> app
    _ -> app

goLiteral :: LiteralLoc -> Node
goLiteral l = case l ^. withLocParam of
  Internal.LitString s -> mkLitConst (ConstString s)
  Internal.LitInteger i -> mkLitConst (ConstInteger i)
  where
    mkLitConst :: ConstantValue -> Node
    mkLitConst = mkConstant (Info.singleton (LocationInfo (l ^. withLocInt)))
