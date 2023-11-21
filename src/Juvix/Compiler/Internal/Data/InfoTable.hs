module Juvix.Compiler.Internal.Data.InfoTable
  ( module Juvix.Compiler.Store.Internal.Language,
    buildInfoTable,
    computeInternalModule,
    extendWithReplExpression,
    lookupConstructor,
    lookupConstructorArgTypes,
    lookupFunction,
    lookupConstructorReturnType,
    lookupInductive,
    lookupAxiom,
    lookupInductiveType,
    lookupConstructorType,
    getAxiomBuiltinInfo,
    getFunctionBuiltinInfo,
    mkConstructorEntries,
    functionInfoFromFunctionDef,
    inductiveInfoFromInductiveDef,
  )
where

import Data.Generics.Uniplate.Data
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.CoercionInfo
import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty (ppTrace)
import Juvix.Compiler.Store.Internal.Data.FunctionsTable
import Juvix.Compiler.Store.Internal.Data.TypesTable
import Juvix.Compiler.Store.Internal.Language
import Juvix.Prelude

functionInfoFromFunctionDef :: FunctionDef -> FunctionInfo
functionInfoFromFunctionDef FunctionDef {..} =
  FunctionInfo
    { _functionInfoName = _funDefName,
      _functionInfoType = _funDefType,
      _functionInfoArgsInfo = _funDefArgsInfo,
      _functionInfoBuiltin = _funDefBuiltin,
      _functionInfoCoercion = _funDefCoercion,
      _functionInfoInstance = _funDefInstance,
      _functionInfoTerminating = _funDefTerminating,
      _functionInfoPragmas = _funDefPragmas
    }

inductiveInfoFromInductiveDef :: InductiveDef -> InductiveInfo
inductiveInfoFromInductiveDef InductiveDef {..} =
  InductiveInfo
    { _inductiveInfoName = _inductiveName,
      _inductiveInfoType = _inductiveType,
      _inductiveInfoBuiltin = _inductiveBuiltin,
      _inductiveInfoParameters = _inductiveParameters,
      _inductiveInfoConstructors = map (^. inductiveConstructorName) _inductiveConstructors,
      _inductiveInfoPositive = _inductivePositive,
      _inductiveInfoTrait = _inductiveTrait,
      _inductiveInfoPragmas = _inductivePragmas
    }

extendWithReplExpression :: Expression -> InfoTable -> InfoTable
extendWithReplExpression e =
  over
    infoFunctions
    ( HashMap.union
        ( HashMap.fromList
            [ (f ^. funDefName, functionInfoFromFunctionDef f)
              | f <- letFunctionDefs e
            ]
        )
    )

letFunctionDefs :: (Data from) => from -> [FunctionDef]
letFunctionDefs e =
  concat
    [ concatMap (toList . flattenClause) _letClauses
      | Let {..} <- universeBi e
    ]
  where
    flattenClause :: LetClause -> NonEmpty FunctionDef
    flattenClause = \case
      LetFunDef f -> pure f
      LetMutualBlock (MutualBlockLet fs) -> fs

buildInfoTable :: InternalModuleTable -> InfoTable
buildInfoTable = mconcatMap (^. internalModuleInfoTable) . HashMap.elems . (^. internalModuleTable)

computeInternalModule :: TypesTable -> FunctionsTable -> Module -> InternalModule
computeInternalModule tysTab funsTab m@Module {..} =
  InternalModule
    { _internalModuleName = _moduleName,
      _internalModuleImports = _moduleBody ^. moduleImports,
      _internalModuleInfoTable = computeInfoTable m,
      _internalModuleTypesTable = tysTab,
      _internalModuleFunctionsTable = funsTab
    }

computeInfoTable :: Module -> InfoTable
computeInfoTable m = InfoTable {..}
  where
    mutuals :: [MutualStatement]
    mutuals =
      [ d
        | MutualBlock b <- ss,
          d <- toList b
      ]

    inductives :: [InductiveDef]
    inductives =
      [ d
        | StatementInductive d <- mutuals
      ]

    _infoInductives :: HashMap Name InductiveInfo
    _infoInductives =
      HashMap.fromList
        [ (d ^. inductiveName, inductiveInfoFromInductiveDef d)
          | d <- inductives
        ]

    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ e
          | d <- inductives,
            e <- mkConstructorEntries d
        ]

    _infoFunctions :: HashMap Name FunctionInfo
    _infoFunctions =
      HashMap.fromList $
        [ (f ^. funDefName, functionInfoFromFunctionDef f)
          | StatementFunction f <- mutuals
        ]
          <> [ (f ^. funDefName, functionInfoFromFunctionDef f)
               | s <- ss,
                 f <- letFunctionDefs s
             ]

    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ (d ^. axiomName, AxiomInfo d)
          | StatementAxiom d <- mutuals
        ]

    _infoBuiltins :: HashMap BuiltinPrim Name
    _infoBuiltins =
      HashMap.fromList $
        mapMaybe goInd (HashMap.elems _infoInductives)
          <> mapMaybe goConstr (HashMap.elems _infoConstructors)
          <> mapMaybe goFun (HashMap.elems _infoFunctions)
          <> mapMaybe goAxiom (HashMap.elems _infoAxioms)
      where
        goInd :: InductiveInfo -> Maybe (BuiltinPrim, Name)
        goInd InductiveInfo {..} =
          _inductiveInfoBuiltin
            >>= (\b -> Just (BuiltinsInductive b, _inductiveInfoName))

        goConstr :: ConstructorInfo -> Maybe (BuiltinPrim, Name)
        goConstr ConstructorInfo {..} =
          _constructorInfoBuiltin
            >>= (\b -> Just (BuiltinsConstructor b, _constructorInfoName))

        goFun :: FunctionInfo -> Maybe (BuiltinPrim, Name)
        goFun FunctionInfo {..} =
          _functionInfoBuiltin
            >>= (\b -> Just (BuiltinsFunction b, _functionInfoName))

        goAxiom :: AxiomInfo -> Maybe (BuiltinPrim, Name)
        goAxiom AxiomInfo {..} =
          _axiomInfoDef ^. axiomBuiltin
            >>= (\b -> Just (BuiltinsAxiom b, _axiomInfoDef ^. axiomName))

    _infoInstances :: InstanceTable
    _infoInstances = foldr (flip updateInstanceTable) mempty $ mapMaybe mkInstance (HashMap.elems _infoFunctions)
      where
        mkInstance :: FunctionInfo -> Maybe InstanceInfo
        mkInstance (FunctionInfo {..})
          | _functionInfoInstance =
              instanceFromTypedExpression
                ( TypedExpression
                    { _typedType = _functionInfoType,
                      _typedExpression = ExpressionIden (IdenFunction _functionInfoName)
                    }
                )
          | otherwise =
              Nothing

    _infoCoercions :: CoercionTable
    _infoCoercions = foldr (flip updateCoercionTable) mempty $ mapMaybe mkCoercion (HashMap.elems _infoFunctions)
      where
        mkCoercion :: FunctionInfo -> Maybe CoercionInfo
        mkCoercion (FunctionInfo {..})
          | _functionInfoCoercion =
              coercionFromTypedExpression
                ( TypedExpression
                    { _typedType = _functionInfoType,
                      _typedExpression = ExpressionIden (IdenFunction _functionInfoName)
                    }
                )
          | otherwise =
              Nothing

    ss :: [MutualBlock]
    ss = m ^. moduleBody . moduleStatements

lookupConstructor :: forall r. (Member (Reader InfoTable) r) => Name -> Sem r ConstructorInfo
lookupConstructor f = do
  err <- impossibleErr
  HashMap.lookupDefault err f <$> asks (^. infoConstructors)
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoConstructors)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> "The registered constructors are: "
          <> ppTrace (HashMap.keys tbl)

lookupConstructorArgTypes :: (Member (Reader InfoTable) r) => Name -> Sem r ([VarName], [Expression])
lookupConstructorArgTypes = fmap constructorArgTypes . lookupConstructor

lookupInductive :: forall r. (Member (Reader InfoTable) r) => InductiveName -> Sem r InductiveInfo
lookupInductive f = do
  err <- impossibleErr
  HashMap.lookupDefault err f <$> asks (^. infoInductives)
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoInductives)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> "The registered inductives are: "
          <> ppTrace (HashMap.keys tbl)

lookupFunction :: forall r. (Member (Reader InfoTable) r) => Name -> Sem r FunctionInfo
lookupFunction f = do
  err <- impossibleErr
  HashMap.lookupDefault err f <$> asks (^. infoFunctions)
  where
    impossibleErr :: Sem r a
    impossibleErr = do
      tbl <- asks (^. infoFunctions)
      return
        . error
        $ "impossible: "
          <> ppTrace f
          <> " is not in the InfoTable\n"
          <> ppTrace (getLoc f)
          <> "The registered functions are: "
          <> ppTrace (HashMap.keys tbl)

lookupAxiom :: (Member (Reader InfoTable) r) => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks (^. infoAxioms)

lookupInductiveType :: (Member (Reader InfoTable) r) => Name -> Sem r Expression
lookupInductiveType v = do
  info <- lookupInductive v
  let ps = info ^. inductiveInfoParameters
  return $
    foldr
      (\_ k -> uni --> k)
      (smallUniverseE (getLoc v))
      ps
  where
    uni = smallUniverseE (getLoc v)

lookupConstructorType :: (Member (Reader InfoTable) r) => ConstrName -> Sem r Expression
lookupConstructorType = fmap constructorType . lookupConstructor

lookupConstructorReturnType :: (Member (Reader InfoTable) r) => ConstrName -> Sem r Expression
lookupConstructorReturnType = fmap constructorReturnType . lookupConstructor

getAxiomBuiltinInfo :: (Member (Reader InfoTable) r) => Name -> Sem r (Maybe BuiltinAxiom)
getAxiomBuiltinInfo n = do
  maybeAxiomInfo <- HashMap.lookup n <$> asks (^. infoAxioms)
  return $ case maybeAxiomInfo of
    Just axiomInfo -> axiomInfo ^. axiomInfoDef . axiomBuiltin
    Nothing -> Nothing

getFunctionBuiltinInfo :: (Member (Reader InfoTable) r) => Name -> Sem r (Maybe BuiltinFunction)
getFunctionBuiltinInfo n = do
  maybeFunInfo <- HashMap.lookup n <$> asks (^. infoFunctions)
  return $ case maybeFunInfo of
    Just funInfo -> funInfo ^. functionInfoBuiltin
    Nothing -> Nothing

mkConstructorEntries :: InductiveDef -> [(ConstructorName, ConstructorInfo)]
mkConstructorEntries d =
  [ (c ^. inductiveConstructorName, ConstructorInfo {..})
    | let _constructorInfoInductive = d ^. inductiveName
          n = length (d ^. inductiveConstructors)
          _constructorInfoInductiveParameters = d ^. inductiveParameters
          builtins = maybe (replicate n Nothing) (map Just . builtinConstructors) (d ^. inductiveBuiltin),
      (_constructorInfoBuiltin, c) <- zipExact builtins (d ^. inductiveConstructors),
      let _constructorInfoType = c ^. inductiveConstructorType,
      let _constructorInfoName = c ^. inductiveConstructorName,
      let _constructorInfoTrait = d ^. inductiveTrait
  ]
