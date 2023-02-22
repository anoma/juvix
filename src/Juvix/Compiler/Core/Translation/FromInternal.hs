module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Data.Loc qualified as Loc
import Juvix.Extra.Strings qualified as Str

-- Translation of a Name into the identifier index used in the Core InfoTable
mkIdentIndex :: Name -> Text
mkIdentIndex = show . (^. Internal.nameId . Internal.unNameId)

setupIntToNat :: Symbol -> InfoTable -> InfoTable
setupIntToNat sym tab =
  tab
    { _infoIdentifiers = HashMap.insert sym ii (tab ^. infoIdentifiers),
      _identContext = HashMap.insert sym node (tab ^. identContext),
      _infoIntToNat = Just sym
    }
  where
    ii =
      IdentifierInfo
        { _identifierSymbol = sym,
          _identifierName = "intToNat",
          _identifierLocation = Nothing,
          _identifierArgsNum = 1,
          _identifierArgsInfo =
            [ ArgumentInfo
                { _argumentName = "x",
                  _argumentLocation = Nothing,
                  _argumentType = mkTypePrim' (PrimInteger $ PrimIntegerInfo Nothing Nothing),
                  _argumentIsImplicit = Explicit
                }
            ],
          _identifierType = mkPi' mkTypeInteger' mkTypeInteger',
          _identifierIsExported = False,
          _identifierBuiltin = Nothing
        }
    node =
      case (tagZeroM, tagSucM, boolSymM) of
        (Just tagZero, Just tagSuc, Just boolSym) ->
          mkLambda' mkTypeInteger' $
            mkIf'
              boolSym
              (mkBuiltinApp' OpEq [mkVar' 0, mkConstant' (ConstInteger 0)])
              (mkConstr (setInfoName "zero" mempty) tagZero [])
              (mkConstr (setInfoName "suc" mempty) tagSuc [mkApp' (mkIdent' sym) (mkBuiltinApp' OpIntSub [mkVar' 0, mkConstant' (ConstInteger 1)])])
        _ ->
          mkLambda' mkTypeInteger' $ mkVar' 0
    tagZeroM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatZero
    tagSucM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatSuc
    boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinBool

fromInternal :: Internal.InternalTypedResult -> Sem k CoreResult
fromInternal i = do
  (res, _) <- runInfoTableBuilder tab0 (runReader (i ^. InternalTyped.resultFunctions) (runReader (i ^. InternalTyped.resultIdenTypes) f))
  return $
    CoreResult
      { _coreResultTable = setupIntToNat intToNatSym res,
        _coreResultInternalTypedResult = i
      }
  where
    tab0 :: InfoTable
    tab0 = emptyInfoTable {_infoIntToNat = Just intToNatSym, _infoNextSymbol = intToNatSym + 1}

    intToNatSym :: Symbol
    intToNatSym = 0

    f :: (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable] r) => Sem r ()
    f = do
      let resultModules = toList (i ^. InternalTyped.resultModules)
      runReader (Internal.buildTable resultModules) (mapM_ coreModule resultModules)
      where
        coreModule :: (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) => Internal.Module -> Sem r ()
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
              (res ^. coreResultInternalTypedResult . InternalTyped.resultFunctions)
              ( runReader
                  (res ^. coreResultInternalTypedResult . InternalTyped.resultIdenTypes)
                  (runReader initIndexTable (goExpression exp))
              )
          )
      )

registerInductiveDefs ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.Module ->
  Sem r ()
registerInductiveDefs m = registerInductiveDefsBody (m ^. Internal.moduleBody)

registerInductiveDefsBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
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
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.Module ->
  Sem r ()
registerFunctionDefs m = registerFunctionDefsBody (m ^. Internal.moduleBody)

registerFunctionDefsBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
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
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.InductiveDef ->
  Sem r ()
goInductiveDef i = do
  sym <- freshSymbol
  let params =
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
          { _inductiveName = i ^. Internal.inductiveName . nameText,
            _inductiveLocation = Just $ i ^. Internal.inductiveName . nameLoc,
            _inductiveSymbol = sym,
            _inductiveKind = mkSmallUniv,
            _inductiveConstructors = [],
            _inductiveParams = params,
            _inductivePositive = i ^. Internal.inductivePositive,
            _inductiveBuiltin = fmap BuiltinTypeInductive (i ^. Internal.inductiveBuiltin)
          }
      idx = mkIdentIndex (i ^. Internal.inductiveName)
  -- The inductive needs to be registered before translating the constructors,
  -- because their types refer to the inductive
  registerInductive idx info
  ctorInfos <- mapM (goConstructor sym) (i ^. Internal.inductiveConstructors)
  registerInductive idx info {_inductiveConstructors = ctorInfos}

goConstructor ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable] r) =>
  Symbol ->
  Internal.InductiveConstructorDef ->
  Sem r ConstructorInfo
goConstructor sym ctor = do
  mblt <- mBuiltin
  tag <- ctorTag mblt
  ty <- ctorType
  argsNum' <- argsNum

  let info =
        ConstructorInfo
          { _constructorName = ctorName ^. nameText,
            _constructorLocation = Just $ ctorName ^. nameLoc,
            _constructorTag = tag,
            _constructorType = ty,
            _constructorArgsNum = argsNum',
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

    ctorName :: Internal.Name
    ctorName = ctor ^. Internal.inductiveConstructorName

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
        ( Internal.constructorType ctorName
            >>= goType
        )

    argsNum :: Sem r Int
    argsNum = do
      (indParams, ctorArgs) <- InternalTyped.lookupConstructorArgTypes ctorName
      return (length indParams + length ctorArgs)

goMutualBlock ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.MutualBlock ->
  Sem r ()
goMutualBlock m = do
  funcsWithSym <- mapM withSym (m ^. Internal.mutualFunctions)
  tys <- mapM goFunctionDefIden funcsWithSym
  mapM_ goFunctionDef (zipExact (toList funcsWithSym) (toList tys))
  where
    withSym :: a -> Sem r (a, Symbol)
    withSym x = do
      sym <- freshSymbol
      return (x, sym)

goType ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader IndexTable] r) =>
  Internal.Expression ->
  Sem r Type
goType ty = do
  normTy <- evalState InternalTyped.iniState (InternalTyped.strongNormalize' ty)
  squashApps <$> goExpression normTy

goFunctionDefIden ::
  forall r.
  (Members '[InfoTableBuilder, Reader Internal.InfoTable, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable] r) =>
  (Internal.FunctionDef, Symbol) ->
  Sem r Type
goFunctionDefIden (f, sym) = do
  funTy <- runReader initIndexTable (goType (f ^. Internal.funDefType))
  let info =
        IdentifierInfo
          { _identifierName = f ^. Internal.funDefName . nameText,
            _identifierLocation = Just $ f ^. Internal.funDefName . nameLoc,
            _identifierSymbol = sym,
            _identifierType = funTy,
            -- _identiferArgsNum needs to match the number of lambdas in the
            -- body. This needs to be filled in later (in goFunctionDef).
            _identifierArgsNum = 0,
            _identifierArgsInfo = [],
            _identifierIsExported = False,
            _identifierBuiltin = f ^. Internal.funDefBuiltin
          }
  registerIdent (mkIdentIndex (f ^. Internal.funDefName)) info
  when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)
  return funTy

goFunctionDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  ((Internal.FunctionDef, Symbol), Type) ->
  Sem r ()
goFunctionDef ((f, sym), ty) = do
  mbody <- case f ^. Internal.funDefBuiltin of
    Just Internal.BuiltinBoolIf -> return Nothing
    Just _ -> Just <$> runReader initIndexTable (mkFunBody ty f)
    Nothing -> Just <$> runReader initIndexTable (mkFunBody ty f)
  forM_ mbody (registerIdentNode sym)
  forM_ mbody setIdentArgsInfo'
  where
    setIdentArgsInfo' :: Node -> Sem r ()
    setIdentArgsInfo' node = do
      let (is, _) = unfoldLambdas node
      setIdentArgsInfo sym (map (argumentInfoFromBinder . (^. lambdaLhsBinder)) is)

mkFunBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Type -> -- converted type of the function
  Internal.FunctionDef ->
  Sem r Node
mkFunBody ty f =
  mkBody ty (fmap (\c -> (c ^. Internal.clausePatterns, c ^. Internal.clauseBody)) (f ^. Internal.funDefClauses))

mkBody ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Type -> -- type of the function
  NonEmpty ([Internal.PatternArg], Internal.Expression) ->
  Sem r Node
mkBody ty clauses
  | nPatterns == 0 = goExpression (snd (head clauses))
  | otherwise = do
      let values = mkVar Info.empty <$> vs
          argtys = take nPatterns (typeArgs ty)
          values' = map fst $ filter (isInductive . snd) (zipExact values argtys)
          argtys' = filter isInductive argtys
          returnType = mkPis' (drop nPatterns (typeArgs ty)) (typeTarget ty)
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
          return $ foldr mkLambda' body' argtys
        _ : _ -> do
          varsNum <- asks (^. indexTableVarsNum)
          ms <- underBinders nPatterns (mapM (uncurry (goClause varsNum)) clauses)
          let match = mkMatch' (fromList argtys') returnType (fromList values') (toList ms)
          return $ foldr mkLambda' match argtys
  where
    -- Assumption: All clauses have the same number of patterns
    nPatterns :: Int
    nPatterns = checkPatternsNum (length (fst (head clauses))) (NonEmpty.tail $ fmap fst clauses)

    vs :: [Index]
    vs = reverse (take nPatterns [0 ..])

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
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Case ->
  Sem r Node
goCase c = do
  expr <- goExpression (c ^. Internal.caseExpression)
  ty <- goType (fromJust $ c ^. Internal.caseExpressionType)
  case ty of
    NTyp {} -> do
      branches <- toList <$> mapM (goCaseBranch ty) (c ^. Internal.caseBranches)
      return (mkMatch' (NonEmpty.singleton ty) mkDynamic' (pure expr) branches) -- TODO: remove mkDynamic' as soon as we have the type information in Internal
    _ ->
      case c ^. Internal.caseBranches of
        Internal.CaseBranch {..} :| _ ->
          case _caseBranchPattern ^. Internal.patternArgPattern of
            Internal.PatternVariable {} -> do
              vars <- asks (^. indexTableVars)
              varsNum <- asks (^. indexTableVarsNum)
              let vars' = addPatternVariableNames _caseBranchPattern varsNum vars
              body <-
                local
                  (set indexTableVars vars')
                  (underBinders 1 (goExpression _caseBranchExpression))
              return $ mkLet' ty expr body
            _ ->
              impossible
  where
    goCaseBranch :: Type -> Internal.CaseBranch -> Sem r MatchBranch
    goCaseBranch ty b = goPatternArgs 0 (b ^. Internal.caseBranchExpression) [b ^. Internal.caseBranchPattern] [ty]

goLambda ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Lambda ->
  Sem r Node
goLambda l = do
  ty <- goType (fromJust (l ^. Internal.lambdaType))
  mkBody ty (fmap (\c -> (toList (c ^. Internal.lambdaPatterns), c ^. Internal.lambdaBody)) (l ^. Internal.lambdaClauses))

goLet ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Let ->
  Sem r Node
goLet l = do
  vars <- asks (^. indexTableVars)
  varsNum <- asks (^. indexTableVarsNum)
  let bs :: [Name]
      bs = map (\(Internal.LetFunDef (Internal.FunctionDef {..})) -> _funDefName) (toList $ l ^. Internal.letClauses)
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameId) k vs, k + 1)
          )
          (vars, varsNum)
          bs
  (defs, value) <-
    local
      (set indexTableVars vars' . set indexTableVarsNum varsNum')
      ( do
          a <- mapM goLetClause (l ^. Internal.letClauses)
          b <- goExpression (l ^. Internal.letExpression)
          return (a, b)
      )
  return $ mkLetRec' defs value

goLetClause ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.LetClause ->
  Sem r (Type, Node)
goLetClause (Internal.LetFunDef f) = do
  funTy <- goType (f ^. Internal.funDefType)
  funBody <- mkFunBody funTy f
  return (funTy, funBody)

goAxiomInductive ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
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
      Internal.BuiltinIO -> registerInductiveAxiom Nothing builtinIOConstrs
      Internal.BuiltinTrace -> return ()
      Internal.BuiltinFail -> return ()
      Internal.BuiltinStringConcat -> return ()
      Internal.BuiltinStringEq -> return ()
      Internal.BuiltinStringToNat -> return ()
      Internal.BuiltinNatToString -> return ()

    registerInductiveAxiom :: Maybe BuiltinAxiom -> [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] -> Sem r ()
    registerInductiveAxiom ax ctrs = do
      sym <- freshSymbol
      let ty = mkTypeConstr' sym []
          ctrs' = builtinConstrs sym ty ctrs
          info =
            InductiveInfo
              { _inductiveName = a ^. Internal.axiomName . nameText,
                _inductiveLocation = Just $ a ^. Internal.axiomName . nameLoc,
                _inductiveSymbol = sym,
                _inductiveKind = mkSmallUniv,
                _inductiveConstructors = ctrs',
                _inductiveParams = [],
                _inductivePositive = False,
                _inductiveBuiltin = fmap BuiltinTypeAxiom ax
              }
      registerInductive (mkIdentIndex (a ^. Internal.axiomName)) info
      mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) ctrs'

goAxiomDef ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable] r) =>
  Internal.AxiomDef ->
  Sem r ()
goAxiomDef a = do
  boolSym <- getBoolSymbol
  natSym <- getNatSymbol
  case a ^. Internal.axiomBuiltin >>= builtinBody boolSym natSym of
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
      let (is, _) = unfoldLambdas body
      setIdentArgsInfo sym (map (argumentInfoFromBinder . (^. lambdaLhsBinder)) is)
    Nothing -> return ()
  where
    builtinBody :: Symbol -> Symbol -> Internal.BuiltinAxiom -> Maybe Node
    builtinBody boolSym natSym = \case
      Internal.BuiltinNatPrint -> Just $ writeLambda (mkTypeConstr' natSym [])
      Internal.BuiltinStringPrint -> Just $ writeLambda mkTypeString'
      Internal.BuiltinBoolPrint -> Just $ writeLambda mkTypeBool'
      Internal.BuiltinIOSequence -> Nothing
      Internal.BuiltinIOReadline ->
        Just
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
        Just (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpStrConcat [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringEq ->
        Just (mkLambda' mkTypeString' (mkLambda' mkTypeString' (mkBuiltinApp' OpEq [mkVar' 1, mkVar' 0])))
      Internal.BuiltinStringToNat -> do
        Just
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
      Internal.BuiltinNatToString ->
        Just (mkLambda' (mkTypeConstr' natSym []) (mkBuiltinApp' OpShow [mkVar' 0]))
      Internal.BuiltinString -> Nothing
      Internal.BuiltinIO -> Nothing
      Internal.BuiltinTrace -> Nothing
      Internal.BuiltinFail ->
        Just (mkLambda' mkSmallUniv (mkLambda' (mkVar' 0) (mkBuiltinApp' OpFail [mkVar' 0])))

    axiomType' :: Sem r Type
    axiomType' = runReader initIndexTable (goType (a ^. Internal.axiomType))

    writeLambda :: Type -> Node
    writeLambda ty = mkLambda' ty (mkConstr' (BuiltinTag TagWrite) [mkVar' 0])

fromPatternArg ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.PatternArg ->
  Sem r Pattern
fromPatternArg pa = case pa ^. Internal.patternArgName of
  Just pan -> do
    ty <- getPatternType pan
    wrapAsPattern pan ty <$> subPat
  Nothing -> subPat
  where
    subPat :: Sem r Pattern
    subPat = fromPattern (pa ^. Internal.patternArgPattern)

    wrapAsPattern :: Name -> Type -> Pattern -> Pattern
    wrapAsPattern pan ty pat =
      ( PatBinder
          ( PatternBinder
              { _patternBinder =
                  Binder
                    { _binderName = pan ^. nameText,
                      _binderLocation = Just (pan ^. nameLoc),
                      _binderType = ty
                    },
                _patternBinderPattern = pat
              }
          )
      )

    getPatternType :: Name -> Sem r Type
    getPatternType n = asks (fromJust . HashMap.lookup n) >>= goType

    fromPattern :: Internal.Pattern -> Sem r Pattern
    fromPattern = \case
      Internal.PatternVariable n -> do
        ty <- getPatternType n
        return $ PatBinder (PatternBinder (Binder (n ^. nameText) (Just (n ^. nameLoc)) ty) wildcard)
      Internal.PatternConstructorApp c -> do
        (indParams, _) <- InternalTyped.lookupConstructorArgTypes n
        patternArgs <- mapM fromPatternArg params
        let indArgs = replicate (length indParams) wildcard
            args = indArgs ++ patternArgs
        m <- getIdent identIndex
        case m of
          Just (IdentConstr tag) -> return $ PatConstr (PatternConstr (setInfoLocation (n ^. nameLoc) (setInfoName (n ^. nameText) Info.empty)) tag args)
          Just _ -> error ("internal to core: not a constructor " <> txt)
          Nothing -> error ("internal to core: undeclared identifier: " <> txt)
        where
          n :: Name
          n = c ^. Internal.constrAppConstructor

          params :: [Internal.PatternArg]
          params = (c ^. Internal.constrAppParameters)

          identIndex :: Text
          identIndex = mkIdentIndex (c ^. Internal.constrAppConstructor)

          txt :: Text
          txt = c ^. Internal.constrAppConstructor . Internal.nameText
      where
        wildcard :: Pattern
        wildcard = PatWildcard (PatternWildcard Info.empty mkSmallUniv)

getPatternArgVars :: Internal.PatternArg -> [Name]
getPatternArgVars pa = case pa ^. Internal.patternArgName of
  Nothing -> subVars
  Just pan -> pan : subVars
  where
    getPatternVars :: Internal.Pattern -> [Name]
    getPatternVars = \case
      Internal.PatternVariable n -> [n]
      Internal.PatternConstructorApp c ->
        concatMap getPatternArgVars (c ^. Internal.constrAppParameters)

    subVars :: [Name]
    subVars = getPatternVars (pa ^. Internal.patternArgPattern)

goPatternArgs ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
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
      (p : ps', NTyp {} : ptys') -> do
        pat <- fromPatternArg p
        vars <- asks (^. indexTableVars)
        varsNum <- asks (^. indexTableVarsNum)
        let bs :: [Name]
            bs = getPatternArgVars p
            (vars', varsNum') =
              foldl'
                ( \(vs, k) name ->
                    (HashMap.insert (name ^. nameId) k vs, k + 1)
                )
                (vars, varsNum)
                bs
        local
          (set indexTableVars vars' . set indexTableVarsNum varsNum')
          (go (lvl + 1) (pat : pats) ps' ptys')
      (p : ps', _ : ptys') ->
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
        return $ MatchBranch Info.empty (fromList (reverse pats)) body'
      _ ->
        impossible

addPatternVariableNames ::
  Internal.PatternArg ->
  Level ->
  HashMap NameId Level ->
  HashMap NameId Level
addPatternVariableNames p lvl vars =
  foldl' (\vs name -> HashMap.insert (name ^. nameId) lvl vs) vars (getPatternArgVars p)

goExpression ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.Expression ->
  Sem r Node
goExpression = \case
  Internal.ExpressionLet l -> goLet l
  Internal.ExpressionLiteral l -> do
    tab <- getInfoTable
    return (goLiteral (fromJust $ tab ^. infoIntToNat) l)
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
        _ -> return ()
      -- if the function was defined by a let, then in Core it is stored in a variable
      vars <- asks (^. indexTableVars)
      case HashMap.lookup id_ vars of
        Nothing -> do
          m <- getIdent identIndex
          return $ case m of
            Just (IdentFun sym) -> mkIdent (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) sym
            Just _ -> error ("internal to core: not a function: " <> txt)
            Nothing -> error ("internal to core: undeclared identifier: " <> txt)
        Just k -> do
          varsNum <- asks (^. indexTableVarsNum)
          return (mkVar (setInfoLocation (n ^. nameLoc) (Info.singleton (NameInfo (n ^. nameText)))) (varsNum - k - 1))
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
      axiomInfoBuiltin <- Internal.getAxiomBuiltinInfo n
      case axiomInfoBuiltin of
        Just Internal.BuiltinIOSequence -> error "internal to core: >> must be called with 2 arguments"
        Just Internal.BuiltinTrace -> error "internal to core: trace must be called with 2 arguments"
        _ -> return ()
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
  Internal.ExpressionCase l -> goCase l
  e@(Internal.ExpressionFunction {}) -> goFunction (Internal.unfoldFunType e)
  Internal.ExpressionHole h -> error ("internal to core: goExpression hole: " <> show (Loc.getLoc h))
  Internal.ExpressionUniverse {} -> return mkSmallUniv

goFunction ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
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
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
  Internal.SimpleLambda ->
  Sem r Node
goSimpleLambda l = do
  ty <- goType (l ^. Internal.slambdaVarType)
  localAddName (l ^. Internal.slambdaVar) (mkLambda' ty <$> goExpression (l ^. Internal.slambdaBody))

goApplication ::
  forall r.
  (Members '[InfoTableBuilder, Reader InternalTyped.TypesTable, Reader InternalTyped.FunctionsTable, Reader Internal.InfoTable, Reader IndexTable] r) =>
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
                      [arg1, mkLambda' (mkTypeConstr' ioSym []) (shift 1 arg2)]
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
            (_ : _ : arg1 : arg2 : xs) ->
              return (mkApps' (mkBuiltinApp' OpTrace [arg1, arg2]) xs)
            _ -> error "internal to core: trace must be called with 2 arguments"
        Just Internal.BuiltinFail -> app
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
        _ -> app
    _ -> app

goLiteral :: Symbol -> LiteralLoc -> Node
goLiteral intToNat l = case l ^. withLocParam of
  Internal.LitString s -> mkLitConst (ConstString s)
  Internal.LitInteger i -> mkApp' (mkIdent' intToNat) (mkLitConst (ConstInteger i))
  where
    mkLitConst :: ConstantValue -> Node
    mkLitConst = mkConstant (Info.singleton (LocationInfo (l ^. withLocInt)))
