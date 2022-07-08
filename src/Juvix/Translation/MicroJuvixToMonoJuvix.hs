module Juvix.Translation.MicroJuvixToMonoJuvix
  ( module Juvix.Translation.MicroJuvixToMonoJuvix,
    module Juvix.Translation.MicroJuvixToMonoJuvix.TypePropagation,
    module Juvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder,
    module Juvix.Syntax.MonoJuvix.MonoJuvixResult,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Internal.NameIdGen
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.InfoTable qualified as Micro
import Juvix.Syntax.MicroJuvix.Language.Extra (mkConcreteType')
import Juvix.Syntax.MicroJuvix.Language.Extra qualified as Micro
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as Micro
import Juvix.Syntax.MonoJuvix.Language.Extra
import Juvix.Syntax.MonoJuvix.MonoJuvixResult
import Juvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder
import Juvix.Translation.MicroJuvixToMonoJuvix.TypePropagation

data PolyIden
  = PolyFunctionIden Micro.FunctionName
  | PolyConstructorIden Micro.ConstructorName
  | PolyInductiveIden Micro.InductiveName
  deriving stock (Eq, Generic)

instance Hashable PolyIden

data ConcreteIdenInfo = ConcreteIdenInfo
  { _concreteName :: Name,
    _concreteTypeSubs :: Micro.ConcreteSubs
  }

data PolyIdenInfo = PolyIdenInfo
  { _polyTypeArity :: Int,
    -- | Each (NonEmpty Micro.ConcreteType) should be of length _polyTypeArity
    _polyConcretes :: HashMap (NonEmpty Micro.ConcreteType) ConcreteIdenInfo
  }

newtype ConcreteTable = ConcreteTable
  { _concreteTable :: HashMap PolyIden PolyIdenInfo
  }

makeLenses ''ConcreteTable
makeLenses ''PolyIdenInfo
makeLenses ''ConcreteIdenInfo

entryMonoJuvix ::
  Members '[NameIdGen] r =>
  Micro.MicroJuvixTypedResult ->
  Sem r MonoJuvixResult
entryMonoJuvix i = do
  concreteTbl <- buildConcreteTable table typesTable
  _resultModules <- runReader infoTable (runReader concreteTbl (mapM goModule (i ^. Micro.resultModules)))
  return MonoJuvixResult {..}
  where
    infoTable = Micro.buildTable (i ^. Micro.resultModules)
    typesTable :: Micro.TypeCalls
    typesTable = collectTypeCalls i
    table :: Micro.InfoTable
    table = Micro.buildTable (i ^. Micro.resultModules)
    _resultMicroTyped = i

cloneName' :: Members '[NameIdGen] r => Micro.Name -> Sem r Micro.Name
cloneName' n = do
  fresh <- freshNameId
  return (set Micro.nameId fresh n)

cloneName :: Members '[NameIdGen] r => Micro.Name -> Sem r Name
cloneName n = goName <$> cloneName' n

addConcreteInfo :: NonEmpty Micro.ConcreteType -> ConcreteIdenInfo -> Maybe PolyIdenInfo -> PolyIdenInfo
addConcreteInfo t c = \case
  Just p
    | p ^. polyTypeArity == length t -> over polyConcretes (HashMap.insert t c) p
    | otherwise -> impossible
  Nothing ->
    PolyIdenInfo
      { _polyTypeArity = length t,
        _polyConcretes = HashMap.singleton t c
      }

buildConcreteTable ::
  forall r.
  Members '[NameIdGen] r =>
  Micro.InfoTable ->
  Micro.TypeCalls ->
  Sem r ConcreteTable
buildConcreteTable info =
  execState (ConcreteTable mempty)
    . mapM_ (uncurry go)
    . HashMap.toList
    . (^. Micro.typeCallSet)
  where
    go ::
      Members '[State ConcreteTable, NameIdGen] k =>
      Micro.TypeCallIden ->
      HashMap Micro.ConcreteTypeCall Micro.ConcreteSubs ->
      Sem k ()
    go i m = case i of
      Micro.FunctionIden fun -> mapM_ (goCFunction fun) (HashMap.toList m)
      Micro.InductiveIden ind -> mapM_ (goCInductive ind) (HashMap.toList m)
    goCFunction ::
      Members '[State ConcreteTable, NameIdGen] k =>
      Micro.FunctionName ->
      (Micro.ConcreteTypeCall, Micro.ConcreteSubs) ->
      Sem k ()
    goCFunction f (tc, s) = do
      f' <- cloneName f
      let fc =
            ConcreteIdenInfo
              { _concreteName = f',
                _concreteTypeSubs = s
              }
          k :: NonEmpty Micro.ConcreteType
          k = tc ^. Micro.typeCallArguments
          iden = PolyFunctionIden f
      modify (over concreteTable (over (at iden) (Just . addConcreteInfo k fc)))
    goCInductive ::
      Members '[State ConcreteTable, NameIdGen] k =>
      Micro.InductiveName ->
      (Micro.ConcreteTypeCall, Micro.ConcreteSubs) ->
      Sem k ()
    goCInductive ind (tc, s) = do
      let def :: Micro.InductiveDef
          def = info ^?! Micro.infoInductives . at ind . _Just . Micro.inductiveInfoDef
          constructorNames :: [Micro.Name]
          constructorNames = def ^.. Micro.inductiveConstructors . each . Micro.constructorName
          k :: NonEmpty Micro.ConcreteType
          k = tc ^. Micro.typeCallArguments
          iden :: PolyIden
          iden = PolyInductiveIden ind
      ind' <- cloneName ind
      mapM_ goCConstructor constructorNames
      let ic =
            ConcreteIdenInfo
              { _concreteName = ind',
                _concreteTypeSubs = s
              }
      modify (over concreteTable (over (at iden) (Just . addConcreteInfo k ic)))
      where
        goCConstructor ::
          Members '[State ConcreteTable, NameIdGen] k =>
          Micro.ConstructorName ->
          Sem k ()
        goCConstructor c = do
          c' <- cloneName c
          let cc =
                ConcreteIdenInfo
                  { _concreteName = c',
                    _concreteTypeSubs = s
                  }
              k :: NonEmpty Micro.ConcreteType
              k = tc ^. Micro.typeCallArguments
              iden :: PolyIden
              iden = PolyConstructorIden c
          modify (over concreteTable (over (at iden) (Just . addConcreteInfo k cc)))

goModule ::
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.Module ->
  Sem r Module
goModule Micro.Module {..} = do
  _moduleBody' <- goModuleBody _moduleBody
  return
    Module
      { _moduleName = goName _moduleName,
        _moduleBody = _moduleBody'
      }

goModuleBody ::
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.ModuleBody ->
  Sem r ModuleBody
goModuleBody b =
  ModuleBody <$> concatMapM goStatement (b ^. Micro.moduleStatements)

goInclude ::
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.Include ->
  Sem r [Statement]
goInclude i =
  (^. moduleStatements) <$> goModuleBody (i ^. Micro.includeModule . Micro.moduleBody)

goStatement ::
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.Statement ->
  Sem r [Statement]
goStatement = \case
  Micro.StatementInductive d -> map StatementInductive <$> goInductive d
  Micro.StatementFunction d -> map StatementFunction <$> goFunctionDef d
  Micro.StatementForeign d -> return [StatementForeign d]
  Micro.StatementAxiom a -> pure . StatementAxiom <$> goAxiomDef a
  Micro.StatementInclude i -> goInclude i

goAxiomDef :: Members '[Reader ConcreteTable] r => Micro.AxiomDef -> Sem r AxiomDef
goAxiomDef Micro.AxiomDef {..} = do
  _axiomType' <- goType (Micro.mkConcreteType' _axiomType)
  let _axiomBuiltin' = _axiomBuiltin
  return
    AxiomDef
      { _axiomName = goName _axiomName,
        _axiomBuiltin = _axiomBuiltin',
        _axiomType = _axiomType'
      }

goName :: Micro.Name -> Name
goName n =
  Name
    { _nameText = n ^. Micro.nameText,
      _nameId = n ^. Micro.nameId,
      _nameLoc = n ^. Micro.nameLoc,
      _nameKind = n ^. Micro.nameKind
    }

lookupPolyConstructor ::
  Members '[Reader ConcreteTable] r =>
  Micro.ConstructorName ->
  Sem r (Maybe PolyIdenInfo)
lookupPolyConstructor i = asks (^. concreteTable . at (PolyConstructorIden i))

lookupPolyInductive ::
  Members '[Reader ConcreteTable] r =>
  Micro.InductiveName ->
  Sem r (Maybe PolyIdenInfo)
lookupPolyInductive i = asks (^. concreteTable . at (PolyInductiveIden i))

lookupPolyFunction ::
  Members '[Reader ConcreteTable] r =>
  Micro.InductiveName ->
  Sem r (Maybe PolyIdenInfo)
lookupPolyFunction i = asks (^. concreteTable . at (PolyFunctionIden i))

goFunctionDef ::
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.FunctionDef ->
  Sem r [FunctionDef]
goFunctionDef def = do
  m <- lookupPolyFunction (def ^. Micro.funDefName)
  case m of
    Just polyInfo -> goFunctionDefPoly def polyInfo
    Nothing -> case Micro.mkConcreteType (def ^. Micro.funDefType) of
      -- The function is either never called and has a polymrphic type. We can ignore it.
      Nothing -> return []
      -- the function has a concrete type
      Just {} -> pure <$> goFunctionDefConcrete Nothing def

goInductive ::
  forall r.
  Members '[Reader ConcreteTable, NameIdGen] r =>
  Micro.InductiveDef ->
  Sem r [InductiveDef]
goInductive def = do
  m <- lookupPolyInductive (def ^. Micro.inductiveName)
  case m of
    Just polyInfo -> goInductiveDefPoly def polyInfo
    Nothing
      | null (def ^. Micro.inductiveParameters) -> pure <$> goInductiveDefConcrete def
      | otherwise -> return []

goInductiveDefConcrete ::
  forall r.
  Members '[Reader ConcreteTable] r =>
  Micro.InductiveDef ->
  Sem r InductiveDef
goInductiveDefConcrete def = do
  constructors' <- mapM goConstructor (def ^. Micro.inductiveConstructors)
  return
    InductiveDef
      { _inductiveName = goName (def ^. Micro.inductiveName),
        _inductiveBuiltin = def ^. Micro.inductiveBuiltin,
        _inductiveConstructors = constructors'
      }
  where
    goConstructor :: Micro.InductiveConstructorDef -> Sem r InductiveConstructorDef
    goConstructor c = do
      params' <- mapM (goType . Micro.mkConcreteType') (c ^. Micro.constructorParameters)
      return
        InductiveConstructorDef
          { _constructorName = goName (c ^. Micro.constructorName),
            _constructorParameters = params'
          }

goExpression ::
  forall r.
  Members '[Reader ConcreteTable] r =>
  Micro.Expression ->
  Sem r Expression
goExpression = go
  where
    go :: Micro.Expression -> Sem r Expression
    go = \case
      Micro.ExpressionIden i -> return (ExpressionIden (goIden i))
      Micro.ExpressionLiteral l -> return (ExpressionLiteral l)
      Micro.ExpressionApplication a -> goApp a
      Micro.ExpressionFunction {} -> impossible
      Micro.ExpressionUniverse {} -> impossible
      Micro.ExpressionHole {} -> impossible
    goApp :: Micro.Application -> Sem r Expression
    goApp a = do
      let (f, args) = Micro.unfoldApplication a
      case f of
        Micro.ExpressionLiteral {} -> goExpression f
        _ -> do
          m :: Maybe (PolyIdenInfo, Name -> Iden) <- case f of
            Micro.ExpressionIden (Micro.IdenConstructor c) -> do
              r <- lookupPolyConstructor c
              return $ (\x -> (x, IdenConstructor)) <$> r
            Micro.ExpressionIden (Micro.IdenFunction c) -> do
              r <- lookupPolyFunction c
              return $ (\x -> (x, IdenFunction)) <$> r
            _ -> return Nothing
          case m of
            Nothing -> do
              l' <- go (a ^. Micro.appLeft)
              r' <- go (a ^. Micro.appRight)
              return (ExpressionApplication (Application l' r'))
            Just (poly, mkIden) -> do
              let (headArgs, tailArgs) = splitAtExact (poly ^. polyTypeArity) (toList args)
                  headArgs' :: NonEmpty Micro.ConcreteType
                  headArgs' = fromJust (nonEmpty (map Micro.mkConcreteType' headArgs))
                  conc :: ConcreteIdenInfo
                  conc = fromJust (poly ^. polyConcretes . at headArgs')
                  fun' :: Expression
                  fun' = ExpressionIden (mkIden (conc ^. concreteName))
              tailArgs' <- mapM go tailArgs
              return (foldApplication fun' tailArgs')
    goIden :: Micro.Iden -> Iden
    goIden = \case
      Micro.IdenFunction f -> IdenFunction (goName f)
      Micro.IdenVar v -> IdenVar (goName v)
      Micro.IdenAxiom a -> IdenAxiom (goName a)
      Micro.IdenConstructor c -> IdenConstructor (goName c)
      Micro.IdenInductive {} -> impossible

goFunctionDefConcrete ::
  forall r.
  Members '[Reader ConcreteTable, Reader Micro.InfoTable] r =>
  Maybe Name ->
  Micro.FunctionDef ->
  Sem r FunctionDef
goFunctionDefConcrete n d = do
  type' <- goType concreteTy
  clauses' <- mapM goClause (d ^. Micro.funDefClauses)
  return
    FunctionDef
      { _funDefName = funName,
        _funDefClauses = clauses',
        _funDefType = type',
        _funDefBuiltin = d ^. Micro.funDefBuiltin
      }
  where
    funName :: Name
    funName = fromMaybe (goName (d ^. Micro.funDefName)) n
    concreteTy :: Micro.ConcreteType
    concreteTy = Micro.mkConcreteType' (d ^. Micro.funDefType)
    patternTys :: [Micro.ConcreteType]
    patternTys = map (mkConcreteType' . (^. Micro.paramType)) . fst . Micro.unfoldFunType . (^. Micro.unconcreteType) $ concreteTy
    goClause :: Micro.FunctionClause -> Sem r FunctionClause
    goClause c = do
      body' <- goExpression (c ^. Micro.clauseBody)
      patterns' <- zipWithM goPattern' patternTys (c ^. Micro.clausePatterns)
      return
        FunctionClause
          { _clauseName = funName,
            _clausePatterns = patterns',
            _clauseBody = body'
          }

goInductiveDefPoly ::
  forall r.
  Members '[Reader ConcreteTable, NameIdGen] r =>
  Micro.InductiveDef ->
  PolyIdenInfo ->
  Sem r [InductiveDef]
goInductiveDefPoly def poly
  | length (def ^. Micro.inductiveParameters) /= poly ^. polyTypeArity = impossible
  | otherwise = mapM (uncurry go) (HashMap.toList (poly ^. polyConcretes))
  where
    go :: NonEmpty Micro.ConcreteType -> ConcreteIdenInfo -> Sem r InductiveDef
    go k i = do
      _inductiveConstructors <- mapM goConstructorDef (def ^. Micro.inductiveConstructors)
      return
        InductiveDef
          { _inductiveName = i ^. concreteName,
            _inductiveBuiltin = Nothing,
            ..
          }
      where
        goConstructorDef :: Micro.InductiveConstructorDef -> Sem r InductiveConstructorDef
        goConstructorDef cdef = do
          cpolyInfo <- fromJust <$> lookupPolyConstructor (cdef ^. Micro.constructorName)
          let concrete :: ConcreteIdenInfo
              concrete = fromJust (cpolyInfo ^. polyConcretes . at k)
              params :: [Micro.ConcreteType]
              params =
                map
                  (Micro.substitutionConcrete (concrete ^. concreteTypeSubs))
                  (cdef ^. Micro.constructorParameters)
          _constructorParameters <- mapM goType params
          return
            InductiveConstructorDef
              { _constructorName = concrete ^. concreteName,
                ..
              }

goFunctionDefPoly ::
  forall r.
  Members '[Reader ConcreteTable, NameIdGen, Reader Micro.InfoTable] r =>
  Micro.FunctionDef ->
  PolyIdenInfo ->
  Sem r [FunctionDef]
goFunctionDefPoly def poly
  | length tyVars /= poly ^. polyTypeArity = impossible
  | otherwise = mapM go (toList (poly ^. polyConcretes))
  where
    (tyVars, tyTail) = Micro.unfoldTypeAbsType (def ^. Micro.funDefType)
    go :: ConcreteIdenInfo -> Sem r FunctionDef
    go i = do
      let funName = i ^. concreteName
      _funDefClauses <- mapM goClause (def ^. Micro.funDefClauses)
      goFunctionDefConcrete
        (Just funName)
        Micro.FunctionDef
          { _funDefName = impossible,
            _funDefType = sig' ^. Micro.unconcreteType,
            _funDefClauses = _funDefClauses,
            _funDefBuiltin = def ^. Micro.funDefBuiltin
          }
      where
        concreteSubs :: Micro.ConcreteSubs
        concreteSubs = i ^. concreteTypeSubs
        concreteSubsE :: Micro.SubsE
        concreteSubsE = Micro.concreteSubsToSubsE concreteSubs
        goClause :: Micro.FunctionClause -> Sem r Micro.FunctionClause
        goClause c = do
          pvars' <- mapM cloneName' pvars
          let localVarsRename :: Micro.Rename
              localVarsRename = HashMap.fromList (zipExact pvars pvars')
              _clausePatterns = map (Micro.renamePattern localVarsRename) patsTail
              _clauseBody =
                Micro.substitutionE
                  (concreteSubsE <> Micro.renameToSubsE localVarsRename)
                  (c ^. Micro.clauseBody)
          return
            Micro.FunctionClause
              { _clauseName = impossible,
                ..
              }
          where
            patsTail :: [Micro.Pattern]
            patsTail = dropExact (length tyVars) (c ^. Micro.clausePatterns)
            pvars :: [Micro.VarName]
            pvars = concatMap Micro.patternVariables patsTail
        sig' :: Micro.ConcreteType
        sig' = Micro.substitutionConcrete (i ^. concreteTypeSubs) tyTail

goPattern' :: forall r. Members '[Reader ConcreteTable, Reader Micro.InfoTable] r => Micro.ConcreteType -> Micro.Pattern -> Sem r Pattern
goPattern' ty = \case
  Micro.PatternVariable v -> return (PatternVariable (goName v))
  Micro.PatternConstructorApp capp -> PatternConstructorApp <$> goApp capp
  Micro.PatternWildcard {} -> return PatternWildcard
  Micro.PatternBraces b -> goPattern' ty b
  where
    goApp :: Micro.ConstructorApp -> Sem r ConstructorApp
    goApp capp = case ty ^. Micro.unconcreteType of
      Micro.ExpressionIden Micro.IdenInductive {} -> do
        let c' :: Name
            c' = goName (capp ^. Micro.constrAppConstructor)
        cInfo <- Micro.lookupConstructor (capp ^. Micro.constrAppConstructor)
        let psTysConcrete = map Micro.mkConcreteType' (cInfo ^. Micro.constructorInfoArgs)
        ps' <- zipWithM goPattern' psTysConcrete (capp ^. Micro.constrAppParameters)
        return (ConstructorApp c' ps')
      Micro.ExpressionApplication a -> do
        let getInductive :: Micro.Expression -> Micro.Name
            getInductive = \case
              Micro.ExpressionIden (Micro.IdenInductive i) -> i
              _ -> impossible
            (ind, instanceTypes) :: (Micro.Name, NonEmpty Micro.ConcreteType) =
              bimap getInductive (Micro.mkConcreteType' <$>) (Micro.unfoldApplication a)
        res <- lookupPolyConstructor (capp ^. Micro.constrAppConstructor)
        let c' :: Name
            c' = fromJust $ do
              poly <- res
              info <- poly ^. polyConcretes . at instanceTypes
              return (info ^. concreteName)
        cInfo <- Micro.lookupConstructor (capp ^. Micro.constrAppConstructor)
        iInfo <- Micro.lookupInductive ind
        let psTys = cInfo ^. Micro.constructorInfoArgs
            tyParamVars :: [Micro.VarName]
            tyParamVars = iInfo ^.. Micro.inductiveInfoDef . Micro.inductiveParameters . each . Micro.inductiveParamName
            subs :: Micro.ConcreteSubs
            subs = HashMap.fromList (zipExact tyParamVars (toList instanceTypes))
            psTysConcrete :: [Micro.ConcreteType]
            psTysConcrete = map (Micro.substitutionConcrete subs) psTys
        ps' <- zipWithM goPattern' psTysConcrete (capp ^. Micro.constrAppParameters)
        return (ConstructorApp c' ps')
      _ -> impossible

goType ::
  forall r.
  Members '[Reader ConcreteTable] r =>
  Micro.ConcreteType ->
  Sem r Type
goType = go . (^. Micro.unconcreteType)
  where
    go :: Micro.Expression -> Sem r Type
    go = \case
      Micro.ExpressionIden i -> return (TypeIden (goIden i))
      Micro.ExpressionUniverse {} -> return TypeUniverse
      Micro.ExpressionHole {} -> impossible
      Micro.ExpressionLiteral {} -> impossible
      Micro.ExpressionFunction f -> TypeFunction <$> goFunction f
      Micro.ExpressionApplication a -> goApp a
    goApp :: Micro.Application -> Sem r Type
    goApp a = case f of
      Micro.ExpressionIden (Micro.IdenInductive i) -> do
        info <- fromJust <$> lookupPolyInductive i
        let (headArgs, tailArgs) = splitAtExact (info ^. polyTypeArity) (toList args)
            headArgs' :: NonEmpty Micro.ConcreteType
            headArgs' = fromJust (nonEmpty (map Micro.mkConcreteType' headArgs))
            concrete :: ConcreteIdenInfo
            concrete = fromJust (info ^. polyConcretes . at headArgs')
        unless (null tailArgs) impossible
        return (TypeIden (TypeIdenInductive (concrete ^. concreteName)))
      _ -> impossible
      where
        (f, args) = Micro.unfoldApplication a
    goFunction :: Micro.Function -> Sem r Function
    goFunction (Micro.Function l r) = do
      l' <- go (l ^. Micro.paramType)
      r' <- go r
      return (Function l' r')
    goIden :: Micro.Iden -> TypeIden
    goIden = \case
      Micro.IdenAxiom a -> TypeIdenAxiom (goName a)
      Micro.IdenInductive i -> TypeIdenInductive (goName i)
      Micro.IdenVar {} -> impossible
      Micro.IdenFunction {} -> impossible
      Micro.IdenConstructor {} -> impossible
