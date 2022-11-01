module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Eta (etaExpandApps)
import Juvix.Compiler.Core.Translation.Base
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
              (goExpression 0 mempty exp)
          )
      )

registerInductiveDefs ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
  Internal.Module ->
  Sem r ()
registerInductiveDefs m = registerInductiveDefsBody (m ^. Internal.moduleBody)

registerInductiveDefsBody ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
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
      Internal.StatementAxiom a -> goAxiomDef a
      Internal.StatementInclude i -> mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)
      _ -> return ()

goInductiveDef ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
  Internal.InductiveDef ->
  Sem r ()
goInductiveDef i = do
  sym <- freshSymbol
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  do
    let info =
          InductiveInfo
            { _inductiveName = i ^. Internal.inductiveName,
              _inductiveSymbol = sym,
              _inductiveKind = mkDynamic',
              _inductiveConstructors = ctorInfos,
              _inductiveParams = [],
              _inductivePositive = i ^. Internal.inductivePositive
            }
    registerInductive info

goConstructor ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
  Symbol ->
  Internal.InductiveConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  tag <- ctorTag
  let info =
        ConstructorInfo
          { _constructorName = ctor ^. Internal.inductiveConstructorName,
            _constructorTag = tag,
            _constructorType = mkDynamic',
            _constructorArgsNum = length (ctor ^. Internal.inductiveConstructorParameters),
            _constructorInductive = sym
          }
  registerConstructor info
  return info
  where
    ctorTag :: Sem r Tag
    ctorTag = do
      ctorInfo <- HashMap.lookupDefault impossible (ctor ^. Internal.inductiveConstructorName) <$> asks (^. Internal.infoConstructors)
      case ctorInfo ^. Internal.constructorInfoBuiltin of
        Just Internal.BuiltinBoolTrue -> return (BuiltinTag TagTrue)
        Just Internal.BuiltinBoolFalse -> return (BuiltinTag TagFalse)
        Just Internal.BuiltinNatZero -> freshTag
        Just Internal.BuiltinNatSuc -> freshTag
        Nothing -> freshTag

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
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, NameIdGen] r =>
  (Internal.FunctionDef, Symbol) ->
  Sem r ()
goFunctionDefIden (f, sym) = do
  let info =
        IdentifierInfo
          { _identifierName = Just (f ^. Internal.funDefName),
            _identifierSymbol = sym,
            _identifierType = mkDynamic',
            _identifierArgsNum = 0,
            _identifierArgsInfo = [],
            _identifierIsExported = False
          }
  registerIdent info
  when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)

goFunctionDef ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, NameIdGen, Reader Internal.InfoTable] r =>
  (Internal.FunctionDef, Symbol) ->
  Sem r ()
goFunctionDef (f, sym) = do
  body <- case f ^. Internal.funDefBuiltin of
    Just Internal.BuiltinBoolIf -> do
      let b = mkIf' (mkVar' 2) (mkVar' 1) (mkVar' 0)
      return (mkLambda' (mkLambda' (mkLambda' b)))
    Just Internal.BuiltinNatPlus -> mkBody
    Nothing -> mkBody
  registerIdentNode sym body
  where
    mkBody :: Sem r Node
    mkBody =
      if
          | nExplicitPatterns == 0 -> goExpression 0 HashMap.empty (f ^. Internal.funDefClauses . _head1 . Internal.clauseBody)
          | otherwise -> do
              let vars :: HashMap Text Index
                  vars = HashMap.fromList [(show i, i) | i <- vs]
                  values :: [Node]
                  values = mkVar Info.empty <$> vs
              ms <- mapM (goFunctionClause nExplicitPatterns vars) (f ^. Internal.funDefClauses)
              let match = mkMatch' (fromList values) (toList ms)
              foldr mkLambda match <$> lamArgs
    -- Assumption: All clauses have the same number of patterns
    nExplicitPatterns :: Int
    nExplicitPatterns = length $ filter isExplicit (f ^. Internal.funDefClauses . _head1 . Internal.clausePatterns)

    vs :: [Index]
    vs = take nExplicitPatterns [0 ..]

    mkName :: Text -> Sem r Name
    mkName txt = freshName KNameLocal txt (f ^. Internal.funDefName . Internal.nameLoc)

    lamArgs :: Sem r [Info]
    lamArgs = do
      ns <- mapM mkName (show <$> vs)
      return $ binderNameInfo <$> ns

goAxiomDef ::
  forall r.
  Members '[InfoTableBuilder, NameIdGen, Reader Internal.InfoTable] r =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomDef a = case a ^. Internal.axiomBuiltin >>= builtinBody of
  Just body -> do
    sym <- freshSymbol
    let info =
          IdentifierInfo
            { _identifierName = Just (a ^. Internal.axiomName),
              _identifierSymbol = sym,
              _identifierType = mkDynamic',
              _identifierArgsNum = 0,
              _identifierArgsInfo = [],
              _identifierIsExported = False
            }
    registerIdent info
    registerIdentNode sym body
  Nothing -> return ()
  where
    builtinBody :: Internal.BuiltinAxiom -> Maybe Node
    builtinBody = \case
      Internal.BuiltinNatPrint -> Just writeLambda
      Internal.BuiltinStringPrint -> Just writeLambda
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

    writeLambda :: Node
    writeLambda = mkLambda' (mkConstr' (BuiltinTag TagWrite) [mkVar' 0])

fromPattern ::
  forall r.
  Members '[InfoTableBuilder, Reader Internal.InfoTable] r =>
  Internal.Pattern ->
  Sem r Pattern
fromPattern = \case
  Internal.PatternWildcard {} -> return wildcard
  Internal.PatternVariable n -> return $ PatBinder (PatternBinder (setInfoName n Info.empty) wildcard)
  Internal.PatternConstructorApp c -> do
    let n = c ^. Internal.constrAppConstructor
        explicitPatterns =
          (^. Internal.patternArgPattern)
            <$> filter
              isExplicit
              (c ^. Internal.constrAppParameters)

    args <- mapM fromPattern explicitPatterns
    m <- getIdent txt
    case m of
      Just (IdentConstr tag) -> return $ PatConstr (PatternConstr (setInfoName n Info.empty) tag args)
      Just _ -> error ("internal to core: not a constructor " <> txt)
      Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    where
      txt :: Text
      txt = c ^. Internal.constrAppConstructor . Internal.nameText
  where
    wildcard :: Pattern
    wildcard = PatWildcard (PatternWildcard Info.empty)

goFunctionClause ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.FunctionClause ->
  Sem r MatchBranch
goFunctionClause varsNum vars clause = do
  pats <- patterns
  let pis = concatMap (reverse . getBinderPatternInfos) pats
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameText) k vs, k + 1)
          )
          (vars, varsNum)
          (map (fromJust . getInfoName) pis)

  body <- goExpression varsNum' vars' (clause ^. Internal.clauseBody)
  return $ MatchBranch Info.empty (fromList pats) body
  where
    patterns :: Sem r [Pattern]
    patterns = reverse <$> mapM fromPattern ps

    ps :: [Internal.Pattern]
    ps = (^. Internal.patternArgPattern) <$> filter isExplicit (clause ^. Internal.clausePatterns)

goExpression ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression varsNum vars e = do
  node <- goExpression' varsNum vars e
  tab <- getInfoTable
  return $ etaExpandApps tab node

goExpression' ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression' varsNum vars = \case
  Internal.ExpressionLiteral l -> return (goLiteral l)
  Internal.ExpressionIden i -> case i of
    Internal.IdenVar n -> do
      let k = HashMap.lookupDefault impossible txt vars
      return (mkVar (Info.singleton (NameInfo n)) (varsNum - k - 1))
    Internal.IdenFunction n -> do
      m <- getIdent txt
      return $ case m of
        Just (IdentFun sym) -> mkIdent (Info.singleton (NameInfo n)) sym
        Just _ -> error ("internal to core: not a function: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenInductive {} -> unsupported "goExpression inductive"
    Internal.IdenConstructor n -> do
      m <- getIdent txt
      case m of
        Just (IdentConstr tag) -> return (mkConstr (Info.singleton (NameInfo n)) tag [])
        Just _ -> error ("internal to core: not a constructor " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenAxiom n -> do
      m <- getIdent txt
      return $ case m of
        Just (IdentFun sym) -> mkIdent (Info.singleton (NameInfo n)) sym
        Just _ -> error ("internal to core: not a function: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    where
      txt :: Text
      txt = Internal.getName i ^. Internal.nameText
  Internal.ExpressionApplication a -> goApplication varsNum vars a
  Internal.ExpressionSimpleLambda l -> unsupported ("goExpression simpleLambda: " <> show (Loc.getLoc l))
  Internal.ExpressionLambda l -> unsupported ("goExpression lambda: " <> show (Loc.getLoc l))
  Internal.ExpressionFunction f -> unsupported ("goExpression function: " <> show (Loc.getLoc f))
  Internal.ExpressionHole h -> error ("goExpression hole: " <> show (Loc.getLoc h))
  Internal.ExpressionUniverse u -> error ("goExpression universe: " <> show (Loc.getLoc u))

goApplication ::
  forall r.
  Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader Internal.InfoTable] r =>
  Index ->
  HashMap Text Index ->
  Internal.Application ->
  Sem r Node
goApplication varsNum vars a = do
  (f, args) <- Internal.unfoldPolyApplication a
  let exprArgs :: Sem r [Node]
      exprArgs = mapM (goExpression varsNum vars) args
  fExpr <- goExpression varsNum vars f
  case a ^. Internal.appImplicit of
    Internal.Implicit -> return fExpr
    Internal.Explicit -> mkApps' fExpr <$> exprArgs

goLiteral :: LiteralLoc -> Node
goLiteral l = case l ^. withLocParam of
  Internal.LitString s -> mkLitConst (ConstString s)
  Internal.LitInteger i -> mkLitConst (ConstInteger i)
  where
    mkLitConst :: ConstantValue -> Node
    mkLitConst = mkConstant (Info.singleton (LocationInfo (l ^. withLocInt)))
