module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Info.PragmaInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Translation.FromInternal.Builtins.Int
import Juvix.Compiler.Core.Translation.FromInternal.Builtins.Nat
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Data.Loc qualified as Loc
import Juvix.Data.PPOutput
import Juvix.Extra.Strings qualified as Str

data PreInductiveDef = PreInductiveDef
  { _preInductiveInternal :: Internal.InductiveDef,
    _preInductiveInfo :: InductiveInfo
  }

data PreFunctionDef = PreFunctionDef
  { _preFunInternal :: Internal.FunctionDef,
    _preFunSym :: Symbol,
    _preFunType :: Type
  }

data PreMutual = PreMutual
  { _preInductives :: [PreInductiveDef],
    _preFunctions :: [PreFunctionDef]
  }

makeLenses ''PreMutual

unsupported :: Text -> a
unsupported thing = error ("Internal to Core: Not yet supported: " <> thing)

-- | Translation of a Name into the identifier index used in the Core InfoTable
mkIdentIndex :: Name -> Text
mkIdentIndex = show . (^. Internal.nameId . Internal.unNameId)

fromInternal :: Internal.InternalTypedResult -> Sem k CoreResult
fromInternal i = do
  (res, _) <- runInfoTableBuilder emptyInfoTable (evalState (i ^. InternalTyped.resultFunctions) (runReader (i ^. InternalTyped.resultIdenTypes) f))
  return $
    CoreResult
      { _coreResultTable = res,
        _coreResultInternalTypedResult = i
      }
  where
    f :: Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, State InternalTyped.FunctionsTable] r => Sem r ()
    f = do
      reserveLiteralIntToNatSymbol
      reserveLiteralIntToIntSymbol
      let resultModules = toList (i ^. InternalTyped.resultModules)
      runReader (Internal.buildTable resultModules) (mapM_ goModule resultModules)
      tab <- getInfoTable
      when
        (isNothing (lookupBuiltinInductive tab BuiltinBool))
        declareBoolBuiltins
      setupLiteralIntToNat literalIntToNatNode
      setupLiteralIntToInt literalIntToIntNode

fromInternalExpression :: CoreResult -> Internal.Expression -> Sem r Node
fromInternalExpression res exp = do
  let modules = res ^. coreResultInternalTypedResult . InternalTyped.resultModules
  snd
    <$> runReader
      (Internal.buildTable modules)
      ( runInfoTableBuilder
          (res ^. coreResultTable)
          ( evalState
              (res ^. coreResultInternalTypedResult . InternalTyped.resultFunctions)
              ( runReader
                  (res ^. coreResultInternalTypedResult . InternalTyped.resultIdenTypes)
                  (fromTopIndex (goExpression exp))
              )
          )
      )

goModule ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.Module ->
  Sem r ()
goModule m = mapM_ go (m ^. Internal.moduleBody . Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementAxiom a -> goAxiomInductive a >> goAxiomDef a
      Internal.StatementMutual f -> goMutualBlock f
      Internal.StatementInclude i -> mapM_ go (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

-- | predefine an inductive definition
preInductiveDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.InductiveDef ->
  Sem r PreInductiveDef
preInductiveDef i = do
  sym <- freshSymbol
  let _inductiveName = i ^. Internal.inductiveName . nameText
      params =
        map
          ( \p ->
              ParameterInfo
                { _paramName = p ^. Internal.inductiveParamName . nameText,
                  _paramLocation = Just $ p ^. Internal.inductiveParamName . nameLoc,
                  _paramIsImplicit = False, -- TODO: not currently easily available in Internal
                  _paramKind = mkSmallUniv
                }
          )
          (i ^. Internal.inductiveParameters)
      info =
        InductiveInfo
          { _inductiveLocation = Just $ i ^. Internal.inductiveName . nameLoc,
            _inductiveSymbol = sym,
            _inductiveKind = mkSmallUniv,
            _inductiveConstructors = [],
            _inductiveParams = params,
            _inductivePositive = i ^. Internal.inductivePositive,
            _inductiveBuiltin = fmap BuiltinTypeInductive (i ^. Internal.inductiveBuiltin),
            _inductivePragmas = i ^. Internal.inductivePragmas,
            _inductiveName
          }
      idx = mkIdentIndex (i ^. Internal.inductiveName)
  -- The inductive needs to be registered before translating the constructors,
  -- because their types refer to the inductive
  registerInductive idx info
  return
    PreInductiveDef
      { _preInductiveInfo = info,
        _preInductiveInternal = i
      }

goInductiveDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  PreInductiveDef ->
  Sem r ()
goInductiveDef PreInductiveDef {..} = do
  let i = _preInductiveInternal
      info = _preInductiveInfo
      idx = mkIdentIndex (i ^. Internal.inductiveName)
      sym = info ^. inductiveSymbol
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  registerInductive idx info {_inductiveConstructors = map (^. constructorTag) ctorInfos}

goConstructor ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable] r) =>
  Symbol ->
  Internal.InductiveConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  mblt <- mBuiltin
  tag <- ctorTag mblt
  ty <- ctorType
  argsNum' <- argsNum
  let _constructorName = ctorName ^. nameText
      info =
        ConstructorInfo
          { _constructorLocation = Just $ ctorName ^. nameLoc,
            _constructorTag = tag,
            _constructorType = ty,
            _constructorArgsNum = argsNum',
            _constructorInductive = sym,
            _constructorBuiltin = mblt,
            _constructorFixity = ctorName ^. nameFixity,
            _constructorPragmas = ctor ^. Internal.inductiveConstructorPragmas,
            _constructorName
          }

  registerConstructor (mkIdentIndex (ctor ^. Internal.inductiveConstructorName)) info
  return info
  where
    mBuiltin :: Sem r (Maybe Internal.BuiltinConstructor)
    mBuiltin =
      (^. Internal.constructorInfoBuiltin)
        . HashMap.lookupDefault impossible (ctor ^. Internal.inductiveConstructorName)
        <$> asks (^. Internal.infoConstructors)

    ctorName :: Internal.Name
    ctorName = ctor ^. Internal.inductiveConstructorName

    ctorTag :: Maybe Internal.BuiltinConstructor -> Sem r Tag
    ctorTag = \case
      Just Internal.BuiltinBoolTrue -> return (BuiltinTag TagTrue)
      Just Internal.BuiltinBoolFalse -> return (BuiltinTag TagFalse)
      Just Internal.BuiltinNatZero -> freshTag
      Just Internal.BuiltinNatSuc -> freshTag
      Just Internal.BuiltinIntOfNat -> freshTag
      Just Internal.BuiltinIntNegSuc -> freshTag
      Nothing -> freshTag

    ctorType :: Sem r Type
    ctorType =
      runReader
        initIndexTable
        ( Internal.constructorType ctorName
            >>= goType
        )

    argsNum :: Sem r Int
    argsNum = do
      (indParams, ctorArgs) <- InternalTyped.lookupConstructorArgTypes ctorName
      return (length indParams + length ctorArgs)

goMutualBlock ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.MutualBlock ->
  Sem r ()
goMutualBlock (Internal.MutualBlock m) = preMutual m >>= goMutual
  where
    preMutual :: NonEmpty Internal.MutualStatement -> Sem r PreMutual
    preMutual = execState (PreMutual [] []) . mapM_ step
      where
        step :: Internal.MutualStatement -> Sem (State PreMutual ': r) ()
        step = \case
          Internal.StatementFunction f -> do
            p <- preFunctionDef f
            modify' (over preFunctions (p :))
          Internal.StatementInductive i -> do
            p <- preInductiveDef i
            modify' (over preInductives (p :))

    goMutual :: PreMutual -> Sem r ()
    goMutual PreMutual {..} = do
      forM_ _preInductives goInductiveDef
      forM_ _preFunctions goFunctionDef

preFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable] r) =>
  Internal.FunctionDef ->
  Sem r PreFunctionDef
preFunctionDef f = do
  sym <- freshSymbol
  funTy <- fromTopIndex (goType (f ^. Internal.funDefType))
  let _identifierName = f ^. Internal.funDefName . nameText
      implParamsNum = implicitParametersNum (f ^. Internal.funDefType)
      info =
        IdentifierInfo
          { _identifierName = normalizeBuiltinName (f ^. Internal.funDefBuiltin) (f ^. Internal.funDefName . nameText),
            _identifierLocation = Just $ f ^. Internal.funDefName . nameLoc,
            _identifierSymbol = sym,
            _identifierType = funTy,
            -- _identiferArgsNum needs to match the number of lambdas in the
            -- body. This needs to be filled in later (in goFunctionDef).
            _identifierArgsNum = 0,
            _identifierIsExported = False,
            _identifierBuiltin = f ^. Internal.funDefBuiltin,
            _identifierPragmas =
              over
                pragmasInline
                (fmap (adjustPragmaInline implParamsNum))
                $ over
                  pragmasSpecialiseArgs
                  (fmap (over pragmaSpecialiseArgs (map (implParamsNum +))))
                  (f ^. Internal.funDefPragmas),
            _identifierArgNames = argnames
          }
  case f ^. Internal.funDefBuiltin of
    Just b
      | isIgnoredBuiltin b -> return ()
    _ -> do
      registerIdent (mkIdentIndex (f ^. Internal.funDefName)) info
      when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)
  return
    PreFunctionDef
      { _preFunInternal = f,
        _preFunSym = sym,
        _preFunType = funTy
      }
  where
    argnames :: [Maybe Text]
    argnames = case f ^. Internal.funDefPragmas . pragmasArgNames of
      Just argns ->
        map Just (argns ^. pragmaArgNames)
      Nothing ->
        map
          getPatternName
          (head (f ^. Internal.funDefClauses) ^. Internal.clausePatterns)

    normalizeBuiltinName :: Maybe BuiltinFunction -> Text -> Text
    normalizeBuiltinName blt name = case blt of
      Just b | isNatBuiltin b -> show (pretty b)
      _ -> case name of
        ">" -> Str.natGt
        ">=" -> Str.natGe
        _ -> name

    implicitParametersNum :: Internal.Expression -> Int
    implicitParametersNum = \case
      Internal.ExpressionFunction Internal.Function {..}
        | _functionLeft ^. Internal.paramImplicit == Implicit ->
            implicitParametersNum _functionRight + 1
      _ -> 0

    adjustPragmaInline :: Int -> PragmaInline -> PragmaInline
    adjustPragmaInline n = \case
      InlinePartiallyApplied k -> InlinePartiallyApplied (k + n)
      x -> x

    getPatternName :: Internal.PatternArg -> Maybe Text
    getPatternName pat = case pat ^. Internal.patternArgName of
      Just n -> Just (n ^. nameText)
      Nothing -> case pat ^. Internal.patternArgPattern of
        Internal.PatternVariable n -> Just (n ^. nameText)
        _ -> Nothing

goFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable] r) =>
  PreFunctionDef ->
  Sem r ()
goFunctionDef PreFunctionDef {..} = do
  let f = _preFunInternal
      sym = _preFunSym
      ty = _preFunType
  mbody <- case _preFunInternal ^. Internal.funDefBuiltin of
    Just b
      | isIgnoredBuiltin b -> return Nothing
    _ -> Just <$> fromTopIndex (mkFunBody ty f)
  forM_ mbody (registerIdentNode sym)
  forM_ mbody setIdentArgsInfo'
  where
    setIdentArgsInfo' :: Node -> Sem r ()
    setIdentArgsInfo' node = do
      let (is, _) = unfoldLambdas node
      setIdentArgs _preFunSym (map (^. lambdaLhsBinder) is)

goType ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader IndexTable] r) =>
  Internal.Expression ->
  Sem r Type
goType ty = do
  normTy <- evalState InternalTyped.iniState (InternalTyped.strongNormalize' ty)
  squashApps <$> goExpression normTy

mkFunBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Type -> -- converted type of the function
  Internal.FunctionDef ->
  Sem r Node
mkFunBody ty f =
  mkBody ty (f ^. Internal.funDefName . nameLoc) (fmap (\c -> (c ^. Internal.clausePatterns, c ^. Internal.clauseBody)) (f ^. Internal.funDefClauses))

mkBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Type -> -- type of the function
  Location ->
  NonEmpty ([Internal.PatternArg], Internal.Expression) ->
  Sem r Node
mkBody ty loc clauses
  | nPatterns == 0 = goExpression (snd (head clauses))
  | otherwise = do
      let values = map (mkVar Info.empty) vs
          argtys = take nPatterns (typeArgs ty)
          argbinders = take nPatterns (typeArgsBinders ty)
          values' = map fst $ filter (isInductive . snd) (zipExact values argtys)
          matchArgtys = shiftMatchTypeArg <$> indexFrom 0 argtys
          matchTypeTarget = typeTarget ty
          matchIndArgTys = filter isInductive matchArgtys
          matchReturnType' = mkPis (drop nPatterns (typeArgsBinders ty)) matchTypeTarget
      case values' of
        [] -> do
          vars <- asks (^. indexTableVars)
          varsNum <- asks (^. indexTableVarsNum)
          let (pats, body) = head clauses
              (vars', varsNum') =
                foldl'
                  (\(vrs, k) pat -> (addPatternVariableNames pat k vrs, k + 1))
                  (vars, varsNum)
                  pats
          body' <-
            local
              (set indexTableVars vars' . set indexTableVarsNum varsNum')
              (goExpression body)
          return $ foldr (mkLambda mempty) body' argbinders
        _ : _ -> do
          varsNum <- asks (^. indexTableVarsNum)
          ms <- underBinders nPatterns (mapM (uncurry (goClause varsNum)) clauses)
          let i = setInfoLocation loc mempty
              match = mkMatch i (nonEmpty' matchIndArgTys) matchReturnType' (nonEmpty' values') (toList ms)
          return $ foldr (mkLambda mempty) match argbinders
  where
    -- Assumption: All clauses have the same number of patterns
    nPatterns :: Int
    nPatterns = checkPatternsNum (length (fst (head clauses))) (NonEmpty.tail (fmap fst clauses))

    vs :: [Index]
    vs = reverse (take nPatterns [0 ..])

    -- The types of arguments in the match must be shifted due to the
    -- difference in level between when the type argument in bound (in a
    -- surrounding lambda) and when it is used in the match constructor.
    --
    -- For example:
    --
    --    f : {A : Type} -> A -> List A -> A -> A -> List A;
    --    f _ _ _ := \ { _ := nil };
    --
    -- f has the following type, with indices:
    --
    --    A -> A$0 -> List A$1 -> A$2 -> A$3 -> A$4 -> List A$5
    --
    -- Is translated to the following match (omitting the translation of the body):
    --
    --    λ(? : Type)
    --      λ(? : A$0)
    --        λ(? : List A$1)
    --          λ(? : A$2)
    --            λ(? : A$3)
    --              match (?$2 : List A$4) with : (A$4 → List A$5)
    --
    -- The return type (A$4 -> List A$5) already has the correct indices
    -- relative to the match node. However the type of the match argument has
    -- been shifted by the number of pattern binders below it.
    shiftMatchTypeArg :: Indexed Type -> Type
    shiftMatchTypeArg (Indexed idx ty') = shift (nPatterns - idx) ty'

    checkPatternsNum :: Int -> [[a]] -> Int
    checkPatternsNum len = \case
      [] -> len
      ps : pats | length ps == len -> checkPatternsNum len pats
      _ -> error "internal-to-core: all clauses must have the same number of patterns"

    goClause :: Level -> [Internal.PatternArg] -> Internal.Expression -> Sem r MatchBranch
    goClause lvl pats body = goPatternArgs lvl body pats ptys
      where
        ptys :: [Type]
        ptys = take (length pats) (typeArgs ty)

goCase ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Case ->
  Sem r Node
goCase c = do
  let loc = getLoc c
      i = setInfoLocation loc mempty
  expr <- goExpression (c ^. Internal.caseExpression)
  ty <- goType (fromJust $ c ^. Internal.caseExpressionType)
  case ty of
    NTyp {} -> do
      branches <- toList <$> mapM (goCaseBranch ty) (c ^. Internal.caseBranches)
      rty <- goType (fromJust $ c ^. Internal.caseExpressionWholeType)
      return (mkMatch i (pure ty) rty (pure expr) branches)
    _ ->
      case c ^. Internal.caseBranches of
        Internal.CaseBranch {..} :| _ ->
          case _caseBranchPattern ^. Internal.patternArgPattern of
            Internal.PatternVariable name -> do
              vars <- asks (^. indexTableVars)
              varsNum <- asks (^. indexTableVarsNum)
              let vars' = addPatternVariableNames _caseBranchPattern varsNum vars
              body <-
                local
                  (set indexTableVars vars')
                  (underBinders 1 (goExpression _caseBranchExpression))
              return $ mkLet i (Binder (name ^. nameText) (Just $ name ^. nameLoc) ty) expr body
            _ ->
              impossible
  where
    goCaseBranch :: Type -> Internal.CaseBranch -> Sem r MatchBranch
    goCaseBranch ty b = goPatternArgs 0 (b ^. Internal.caseBranchExpression) [b ^. Internal.caseBranchPattern] [ty]

goLambda ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Lambda ->
  Sem r Node
goLambda l = do
  ty <- goType (fromJust (l ^. Internal.lambdaType))
  mkBody ty (getLoc l) (fmap (\c -> (toList (c ^. Internal.lambdaPatterns), c ^. Internal.lambdaBody)) (l ^. Internal.lambdaClauses))

goLet ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Let ->
  Sem r Node
goLet l = goClauses (toList (l ^. Internal.letClauses))
  where
    goClauses :: [Internal.LetClause] -> Sem r Node
    goClauses = \case
      [] -> goExpression (l ^. Internal.letExpression)
      c : cs -> case c of
        Internal.LetFunDef f -> goNonRecFun f
        Internal.LetMutualBlock m -> goMutual m
        where
          goNonRecFun :: Internal.FunctionDef -> Sem r Node
          goNonRecFun f =
            do
              funTy <- goType (f ^. Internal.funDefType)
              funBody <- mkFunBody funTy f
              rest <- localAddName (f ^. Internal.funDefName) (goClauses cs)
              let name = f ^. Internal.funDefName . nameText
                  loc = f ^. Internal.funDefName . nameLoc
                  info = setInfoPragma (f ^. Internal.funDefPragmas) mempty
                  body = modifyInfo (setInfoPragma (f ^. Internal.funDefPragmas)) funBody
              return $ mkLet info (Binder name (Just loc) funTy) body rest
          goMutual :: Internal.MutualBlockLet -> Sem r Node
          goMutual (Internal.MutualBlockLet funs) = do
            let lfuns = toList funs
                names = map (^. Internal.funDefName) lfuns
                tys = map (^. Internal.funDefType) lfuns
                pragmas = map (^. Internal.funDefPragmas) lfuns
            tys' <- mapM goType tys
            localAddNames names $ do
              vals' <- sequence [mkFunBody (shift (length names) ty) f | (ty, f) <- zipExact tys' lfuns]
              let items = nonEmpty' (zipWith3Exact (\ty n v -> LetItem (Binder (n ^. nameText) (Just $ n ^. nameLoc) ty) v) tys' names vals')
              rest <- goClauses cs
              return (mkLetRec (setInfoPragmas pragmas mempty) items rest)

goAxiomInductive ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
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
      Internal.BuiltinIOReadline -> return ()
      Internal.BuiltinString -> registerInductiveAxiom (Just BuiltinString) []
      Internal.BuiltinIO -> registerInductiveAxiom (Just BuiltinIO) builtinIOConstrs
      Internal.BuiltinTrace -> return ()
      Internal.BuiltinFail -> return ()
      Internal.BuiltinStringConcat -> return ()
      Internal.BuiltinStringEq -> return ()
      Internal.BuiltinStringToNat -> return ()
      Internal.BuiltinNatToString -> return ()
      Internal.BuiltinIntToString -> return ()
      Internal.BuiltinIntPrint -> return ()

    registerInductiveAxiom :: Maybe BuiltinAxiom -> [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] -> Sem r ()
    registerInductiveAxiom ax ctrs = do
      sym <- freshSymbol
      let name = a ^. Internal.axiomName . nameText
          ty = mkTypeConstr (setInfoName name mempty) sym []
          ctrs' = builtinConstrs sym ty ctrs
      let _inductiveName = a ^. Internal.axiomName . nameText
          info =
            InductiveInfo
              { _inductiveLocation = Just $ a ^. Internal.axiomName . nameLoc,
                _inductiveSymbol = sym,
                _inductiveKind = mkSmallUniv,
                _inductiveConstructors = map (^. constructorTag) ctrs',
                _inductiveParams = [],
                _inductivePositive = False,
                _inductiveBuiltin = BuiltinTypeAxiom <$> ax,
                _inductivePragmas = a ^. Internal.axiomPragmas,
                _inductiveName
              }
      registerInductive (mkIdentIndex (a ^. Internal.axiomName)) info
      mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) ctrs'

fromTopIndex :: Sem (Reader IndexTable : r) a -> Sem r a
fromTopIndex = runReader initIndexTable

goAxiomDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomDef a = maybe goAxiomNotBuiltin builtinBody (a ^. Internal.axiomBuiltin)
  where
    goAxiomNotBuiltin :: Sem r ()
    goAxiomNotBuiltin = axiomType' >>= registerAxiomDef . mkAxiom (getLoc a)

    builtinBody :: Internal.BuiltinAxiom -> Sem r ()
    builtinBody = \case
      Internal.BuiltinNatPrint -> do
        natName <- getNatName
        natSym <- getNatSymbol
        registerAxiomDef $ writeLambda (mkTypeConstr (setInfoName natName mempty) natSym [])
      Internal.BuiltinStringPrint -> registerAxiomDef $ writeLambda mkTypeString'
      Internal.BuiltinBoolPrint -> registerAxiomDef $ writeLambda mkTypeBool'
      Internal.BuiltinIOSequence -> return ()
      Internal.BuiltinIOReadline ->
        registerAxiomDef
          ( mkLambda'
              mkTypeString'
              ( mkConstr'
                  (BuiltinTag TagBind)
                  [ mkConstr' (BuiltinTag TagReadLn) [],
                    mkVar' 0
                  ]
              )
          )
      Internal.BuiltinStringConcat ->
        registerAxiomDef (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpStrConcat [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringEq ->
        registerAxiomDef (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpEq [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringToNat -> do
        boolSym <- getBoolSymbol
        registerAxiomDef
          ( mkLambda'
              mkTypeString'
              ( mkLet'
                  mkTypeInteger'
                  (mkBuiltinApp' OpStrToInt [mkVar' 0])
                  ( mkIf'
                      boolSym
                      (mkBuiltinApp' OpIntLt [mkVar' 0, mkConstant' (ConstInteger 0)])
                      (mkBuiltinApp' OpFail [mkConstant' (ConstString "stringToNat: negative value")])
                      (mkVar' 0)
                  )
              )
          )
      Internal.BuiltinNatToString -> do
        natName <- getNatName
        natSym <- getNatSymbol
        registerAxiomDef (mkLambda' (mkTypeConstr (setInfoName natName mempty) natSym []) (mkBuiltinApp' OpShow [mkVar' 0]))
      Internal.BuiltinString -> return ()
      Internal.BuiltinIO -> return ()
      Internal.BuiltinTrace -> return ()
      Internal.BuiltinFail ->
        registerAxiomDef (mkLambda' mkSmallUniv (mkLambda' (mkVar' 0) (mkBuiltinApp' OpFail [mkVar' 0])))
      Internal.BuiltinIntToString -> do
        intName <- getIntName
        intSym <- getIntSymbol
        registerAxiomDef (mkLambda' (mkTypeConstr (setInfoName intName mempty) intSym []) (mkBuiltinApp' OpShow [mkVar' 0]))
      Internal.BuiltinIntPrint -> do
        intName <- getIntName
        intSym <- getIntSymbol
        registerAxiomDef $ writeLambda (mkTypeConstr (setInfoName intName mempty) intSym [])

    axiomType' :: Sem r Type
    axiomType' = fromTopIndex (goType (a ^. Internal.axiomType))

    writeLambda :: Type -> Node
    writeLambda ty = mkLambda' ty (mkConstr' (BuiltinTag TagWrite) [mkVar' 0])

    getNatName :: Sem r Text
    getNatName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinNat

    getIntName :: Sem r Text
    getIntName = (^. inductiveName) <$> getBuiltinInductiveInfo BuiltinInt

    registerAxiomDef :: Node -> Sem r ()
    registerAxiomDef body = do
      let name = a ^. Internal.axiomName
      sym <- freshSymbol
      ty <- axiomType'
      let _identifierName = name ^. nameText
          info =
            IdentifierInfo
              { _identifierLocation = Just $ name ^. nameLoc,
                _identifierSymbol = sym,
                _identifierType = ty,
                _identifierArgsNum = 0,
                _identifierIsExported = False,
                _identifierBuiltin = Nothing,
                _identifierPragmas = a ^. Internal.axiomPragmas,
                _identifierArgNames = [],
                _identifierName
              }
      registerIdent (mkIdentIndex name) info
      registerIdentNode sym body
      let (is, _) = unfoldLambdas body
      setIdentArgs sym (map (^. lambdaLhsBinder) is)

fromPatternArg ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, State IndexTable] r) =>
  Internal.PatternArg ->
  Sem r Pattern
fromPatternArg pa = case pa ^. Internal.patternArgName of
  Just pan -> do
    ty <- getPatternType pan
    subPat $ Just (pan, ty)
  Nothing ->
    subPat Nothing
  where
    subPat :: Maybe (Name, Type) -> Sem r Pattern
    subPat asPat = fromPattern asPat (pa ^. Internal.patternArgPattern)

    getPatternType :: Name -> Sem r Type
    getPatternType n = do
      ty <- asks (fromJust . HashMap.lookup (n ^. nameId))
      idt :: IndexTable <- get
      runReader idt (goType ty)

    fromPattern :: Maybe (Name, Type) -> Internal.Pattern -> Sem r Pattern
    fromPattern asPat = \case
      Internal.PatternVariable n -> do
        ty <- getPatternType n
        varsNum <- (^. indexTableVarsNum) <$> get
        modify
          ( over indexTableVarsNum (+ 1)
              . over indexTableVars (HashMap.insert (n ^. nameId) varsNum)
          )
        case asPat of
          Just (pan, _) -> modify (over indexTableVars (HashMap.insert (pan ^. nameId) varsNum))
          _ -> return ()
        return $ PatWildcard (PatternWildcard mempty (Binder (n ^. nameText) (Just (n ^. nameLoc)) ty))
      Internal.PatternConstructorApp c -> do
        idt :: IndexTable <- get
        ctorTy <- runReader idt $ goType (fromJust (c ^. Internal.constrAppType))
        let varsNum = idt ^. indexTableVarsNum
        case asPat of
          Just (pan, _) -> modify (over indexTableVars (HashMap.insert (pan ^. nameId) varsNum))
          _ -> return ()
        (indParams, _) <- InternalTyped.lookupConstructorArgTypes ctrName
        let nParams = length indParams
        -- + 1 for the as-pattern
        modify (over indexTableVarsNum (+ (nParams + 1)))
        patternArgs <- mapM fromPatternArg params
        let indArgs = replicate nParams (wildcard mkSmallUniv)
            args = indArgs ++ patternArgs
        m <- getIdent identIndex
        case m of
          Just (IdentConstr tag) ->
            return $
              PatConstr
                ( PatternConstr
                    { _patternConstrInfo = setInfoName (ctrName ^. nameText) mempty,
                      _patternConstrBinder = binder ctorTy,
                      _patternConstrTag = tag,
                      _patternConstrArgs = args
                    }
                )
          Just _ -> error ("internal to core: not a constructor " <> txt)
          Nothing -> error ("internal to core: undeclared identifier: " <> txt)
        where
          ctrName :: Name
          ctrName = c ^. Internal.constrAppConstructor

          params :: [Internal.PatternArg]
          params = (c ^. Internal.constrAppParameters)

          identIndex :: Text
          identIndex = mkIdentIndex (c ^. Internal.constrAppConstructor)

          binder :: Type -> Binder
          binder ctorTy = case asPat of
            Just (pan, ty) -> Binder (pan ^. nameText) (Just (pan ^. nameLoc)) ty
            _ -> Binder "_" Nothing ctorTy

          txt :: Text
          txt = c ^. Internal.constrAppConstructor . Internal.nameText

          wildcard :: Type -> Pattern
          wildcard ty = PatWildcard (PatternWildcard Info.empty (Binder "_" Nothing ty))

goPatternArgs ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Level -> -- the level of the first binder for the matched value
  Internal.Expression ->
  [Internal.PatternArg] ->
  [Type] -> -- types of the patterns
  Sem r MatchBranch
goPatternArgs lvl0 body ps0 ptys0 = go lvl0 [] ps0 ptys0
  where
    -- `lvl` is the level of the lambda-bound variable corresponding to the current pattern
    go :: Level -> [Pattern] -> [Internal.PatternArg] -> [Type] -> Sem r MatchBranch
    go lvl pats ps ptys = case (ps, ptys) of
      -- The pattern has an inductive type, so can be matched on
      (p : ps', NTyp {} : ptys') -> do
        itb :: IndexTable <- ask
        (itb', pat) <- runState itb (fromPatternArg p)
        local
          (const itb')
          (go (lvl + 1) (pat : pats) ps' ptys')
      (p : ps', _ : ptys') ->
        -- The pattern does not have an inductive type, so is excluded from the match
        case p ^. Internal.patternArgPattern of
          Internal.PatternVariable {} -> do
            vars <- asks (^. indexTableVars)
            let vars' = addPatternVariableNames p lvl vars
            local
              (set indexTableVars vars')
              (go (lvl + 1) pats ps' ptys')
          _ ->
            impossible
      ([], []) -> do
        body' <- goExpression body
        return $ MatchBranch Info.empty (nonEmpty' (reverse pats)) body'
      _ ->
        impossible

addPatternVariableNames ::
  Internal.PatternArg ->
  Level ->
  HashMap NameId Level ->
  HashMap NameId Level
addPatternVariableNames p lvl vars =
  foldl'
    (\vs -> maybe vs (\name -> HashMap.insert (name ^. nameId) lvl vs))
    vars
    (getPatternArgVars p)
  where
    getPatternArgVars :: Internal.PatternArg -> [Maybe Name]
    getPatternArgVars pa =
      pa ^. Internal.patternArgName : getPatternVars (pa ^. Internal.patternArgPattern)

    getPatternVars :: Internal.Pattern -> [Maybe Name]
    getPatternVars = \case
      Internal.PatternVariable n -> [Just n]
      Internal.PatternConstructorApp {} -> impossible

goExpression ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Expression ->
  Sem r Node
goExpression = \case
  Internal.ExpressionLet l -> goLet l
  Internal.ExpressionLiteral l -> do
    tab <- getInfoTable
    return (goLiteral (fromJust $ tab ^. infoLiteralIntToNat) (fromJust $ tab ^. infoLiteralIntToInt) l)
  Internal.ExpressionIden i -> case i of
    Internal.IdenVar n -> do
      k <- HashMap.lookupDefault impossible id_ <$> asks (^. indexTableVars)
      varsNum <- asks (^. indexTableVarsNum)
      return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
    Internal.IdenFunction n -> do
      funInfoBuiltin <- Internal.getFunctionBuiltinInfo n
      case funInfoBuiltin of
        Just Internal.BuiltinBoolIf -> error "internal to core: if must be called with 3 arguments"
        Just Internal.BuiltinBoolOr -> error "internal to core: || must be called with 2 arguments"
        Just Internal.BuiltinBoolAnd -> error "internal to core: && must be called with 2 arguments"
        Just Internal.BuiltinSeq -> error "internal to core: seq must be called with 2 arguments"
        _ -> return ()
      -- if the function was defined by a let, then in Core it is stored in a variable
      vars <- asks (^. indexTableVars)
      case HashMap.lookup id_ vars of
        Nothing -> do
          m <- getIdent identIndex
          return $ case m of
            Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym
            Just _ -> error ("internal to core: not a function: " <> txt)
            Nothing -> error ("internal to core: undeclared identifier: " <> txt)
        Just k -> do
          varsNum <- asks (^. indexTableVarsNum)
          return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
    Internal.IdenInductive n -> do
      m <- getIdent identIndex
      return $ case m of
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym []
        Just _ -> error ("internal to core: not an inductive: " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenConstructor n -> do
      m <- getIdent identIndex
      case m of
        Just (IdentConstr tag) -> return (mkConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) tag [])
        Just _ -> error ("internal to core: not a constructor " <> txt)
        Nothing -> error ("internal to core: undeclared identifier: " <> txt)
    Internal.IdenAxiom n -> do
      axiomInfoBuiltin <- Internal.getAxiomBuiltinInfo n
      case axiomInfoBuiltin of
        Just Internal.BuiltinIOSequence -> error "internal to core: >> must be called with 2 arguments"
        Just Internal.BuiltinTrace -> error "internal to core: trace must be called with 1 argument"
        _ -> return ()
      m <- getIdent identIndex
      return $ case m of
        Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym
        Just (IdentInd sym) -> mkTypeConstr (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. namePretty)))) sym []
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
  Internal.ExpressionCase l -> goCase l
  e@Internal.ExpressionFunction {} -> goFunction (Internal.unfoldFunType e)
  Internal.ExpressionHole h -> error ("internal to core: goExpression hole: " <> show (Loc.getLoc h))
  Internal.ExpressionUniverse {} -> return mkSmallUniv

goFunction ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  ([Internal.FunctionParameter], Internal.Expression) ->
  Sem r Node
goFunction (params, returnTypeExpr) = go params
  where
    go :: [Internal.FunctionParameter] -> Sem r Node
    go = \case
      param : params' -> do
        paramTy <- goType (param ^. Internal.paramType)
        let paramBinder =
              Binder
                { _binderName = maybe "?" (^. nameText) $ param ^. Internal.paramName,
                  _binderLocation = fmap (^. nameLoc) $ param ^. Internal.paramName,
                  _binderType = paramTy
                }
        case param ^. Internal.paramName of
          Nothing -> mkPi mempty paramBinder <$> local (over indexTableVarsNum (+ 1)) (go params')
          Just vn -> mkPi mempty paramBinder <$> localAddName vn (go params')
      [] ->
        goType returnTypeExpr

goSimpleLambda ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.SimpleLambda ->
  Sem r Node
goSimpleLambda l = do
  ty <- goType (l ^. Internal.slambdaVarType)
  let loc = l ^. Internal.slambdaVar . nameLoc
      name = l ^. Internal.slambdaVar . nameText
  localAddName (l ^. Internal.slambdaVar) (mkLambda mempty (Binder name (Just loc) ty) <$> goExpression (l ^. Internal.slambdaBody))

goApplication ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, State InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Application ->
  Sem r Node
goApplication a = do
  let (f, args) = second toList (Internal.unfoldApplication a)
      exprArgs :: Sem r [Node]
      exprArgs = mapM goExpression args

      app :: Sem r Node
      app = do
        fExpr <- goExpression f
        mkApps' fExpr <$> exprArgs

  case f of
    Internal.ExpressionIden (Internal.IdenAxiom n) -> do
      axiomInfoBuiltin <- Internal.getAxiomBuiltinInfo n
      case axiomInfoBuiltin of
        Just Internal.BuiltinNatPrint -> app
        Just Internal.BuiltinStringPrint -> app
        Just Internal.BuiltinBoolPrint -> app
        Just Internal.BuiltinString -> app
        Just Internal.BuiltinIO -> app
        Just Internal.BuiltinIOSequence -> do
          ioSym <- getIOSymbol
          as <- exprArgs
          case as of
            (arg1 : arg2 : xs) ->
              return $
                mkApps'
                  ( mkConstr'
                      (BuiltinTag TagBind)
                      [arg1, mkLambda' (mkTypeConstr (setInfoName Str.io mempty) ioSym []) (shift 1 arg2)]
                  )
                  xs
            _ -> error "internal to core: >> must be called with 2 arguments"
        Just Internal.BuiltinIOReadline -> app
        Just Internal.BuiltinStringConcat -> app
        Just Internal.BuiltinStringEq -> app
        Just Internal.BuiltinStringToNat -> app
        Just Internal.BuiltinNatToString -> app
        Just Internal.BuiltinTrace -> do
          as <- exprArgs
          case as of
            (_ : arg : xs) ->
              return (mkApps' (mkBuiltinApp' OpTrace [arg]) xs)
            _ -> error "internal to core: trace must be called with 1 argument"
        Just Internal.BuiltinFail -> app
        Just Internal.BuiltinIntToString -> app
        Just Internal.BuiltinIntPrint -> app
        Nothing -> app
    Internal.ExpressionIden (Internal.IdenFunction n) -> do
      funInfoBuiltin <- Internal.getFunctionBuiltinInfo n
      case funInfoBuiltin of
        Just Internal.BuiltinBoolIf -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (_ : v : b1 : b2 : xs) -> return (mkApps' (mkIf' sym v b1 b2) xs)
            _ -> error "internal to core: if must be called with 3 arguments"
        Just Internal.BuiltinBoolOr -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (x : y : xs) -> return (mkApps' (mkIf' sym x (mkConstr' (BuiltinTag TagTrue) []) y) xs)
            _ -> error "internal to core: || must be called with 2 arguments"
        Just Internal.BuiltinBoolAnd -> do
          sym <- getBoolSymbol
          as <- exprArgs
          case as of
            (x : y : xs) -> return (mkApps' (mkIf' sym x y (mkConstr' (BuiltinTag TagFalse) [])) xs)
            _ -> error "internal to core: && must be called with 2 arguments"
        Just Internal.BuiltinSeq -> do
          as <- exprArgs
          case as of
            (_ : _ : arg1 : arg2 : xs) ->
              return (mkApps' (mkBuiltinApp' OpSeq [arg1, arg2]) xs)
            _ -> error "internal to core: seq must be called with 2 arguments"
        _ -> app
    _ -> app

goLiteral :: Symbol -> Symbol -> Internal.LiteralLoc -> Node
goLiteral intToNat intToInt l = case l ^. withLocParam of
  Internal.LitString s -> mkLitConst (ConstString s)
  Internal.LitInteger i -> mkApp' (mkIdent' intToInt) (mkLitConst (ConstInteger i))
  Internal.LitNatural i -> mkApp' (mkIdent' intToNat) (mkLitConst (ConstInteger i))
  where
    mkLitConst :: ConstantValue -> Node
    mkLitConst = mkConstant (Info.singleton (LocationInfo (l ^. withLocInt)))
