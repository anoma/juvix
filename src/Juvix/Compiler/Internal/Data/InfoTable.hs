module Juvix.Compiler.Internal.Data.InfoTable
  ( module Juvix.Compiler.Internal.Data.InfoTable.Base,
    buildTable,
    buildTable1,
    extendWithReplExpression,
    lookupConstructor,
    lookupConstructorArgTypes,
    lookupFunction,
    constructorReturnType,
    constructorArgTypes,
    lookupInductive,
    lookupAxiom,
    lookupInductiveType,
    constructorType,
    getAxiomBuiltinInfo,
    getFunctionBuiltinInfo,
    buildTableShallow,
  )
where

import Data.Generics.Uniplate.Data
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable.Base
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty (ppTrace)
import Juvix.Prelude

type MCache = Cache ModuleIndex InfoTable

buildTable :: Foldable f => f Module -> InfoTable
buildTable = run . evalCache computeTable mempty . getMany

buildTable1 :: Module -> InfoTable
buildTable1 = buildTable . pure @[]

-- TODO do not recurse into imports
buildTableShallow :: Module -> InfoTable
buildTableShallow = buildTable . pure @[]

getMany :: (Members '[MCache] r, Foldable f) => f Module -> Sem r InfoTable
getMany = mconcatMap (cacheGet . ModuleIndex)

extendWithReplExpression :: Expression -> InfoTable -> InfoTable
extendWithReplExpression e =
  over
    infoFunctions
    ( HashMap.union
        ( HashMap.fromList
            [ (f ^. funDefName, FunctionInfo f)
              | f <- letFunctionDefs e
            ]
        )
    )

letFunctionDefs :: Data from => from -> [FunctionDef]
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

computeTable :: forall r. (Members '[MCache] r) => ModuleIndex -> Sem r InfoTable
computeTable (ModuleIndex m) = compute
  where
    compute :: Sem r InfoTable
    compute = do
      infoInc <- mconcatMapM (cacheGet . (^. importModule)) imports
      return (InfoTable {..} <> infoInc)

    imports :: [Import]
    imports = m ^. moduleBody . moduleImports

    mutuals :: [MutualStatement]
    mutuals =
      [ d
        | StatementMutual (MutualBlock b) <- ss,
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
        [ (d ^. inductiveName, InductiveInfo d)
          | d <- inductives
        ]

    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ (c ^. inductiveConstructorName, ConstructorInfo params ty ind builtin)
          | d <- inductives,
            let ind = d ^. inductiveName
                n = length (d ^. inductiveConstructors)
                params = d ^. inductiveParameters
                builtins = maybe (replicate n Nothing) (map Just . builtinConstructors) (d ^. inductiveBuiltin),
            (builtin, c) <- zipExact builtins (d ^. inductiveConstructors),
            let ty = c ^. inductiveConstructorType
        ]

    _infoFunctions :: HashMap Name FunctionInfo
    _infoFunctions =
      HashMap.fromList $
        [ (f ^. funDefName, FunctionInfo f)
          | StatementFunction f <- mutuals
        ]
          <> [ (f ^. funDefName, FunctionInfo f)
               | s <- ss,
                 f <- letFunctionDefs s
             ]

    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ (d ^. axiomName, AxiomInfo d)
          | StatementAxiom d <- ss
        ]

    ss :: [Statement]
    ss = m ^. moduleBody . moduleStatements

lookupConstructor :: forall r. Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
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

lookupInductive :: forall r. Member (Reader InfoTable) r => InductiveName -> Sem r InductiveInfo
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

lookupFunction :: forall r. Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
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
          <> "The registered functions are: "
          <> ppTrace (HashMap.keys tbl)

lookupAxiom :: (Member (Reader InfoTable) r) => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks (^. infoAxioms)

lookupInductiveType :: Member (Reader InfoTable) r => Name -> Sem r Expression
lookupInductiveType v = do
  info <- lookupInductive v
  let ps = info ^. inductiveInfoDef . inductiveParameters
  return $
    foldr
      (\_ k -> uni --> k)
      (smallUniverseE (getLoc v))
      ps
  where
    uni = smallUniverseE (getLoc v)

constructorArgTypes :: ConstructorInfo -> ([VarName], [Expression])
constructorArgTypes i =
  ( map (^. inductiveParamName) (i ^. constructorInfoInductiveParameters),
    constructorArgs (i ^. constructorInfoType)
  )

constructorType :: Member (Reader InfoTable) r => ConstrName -> Sem r Expression
constructorType c = do
  info <- lookupConstructor c
  let (inductiveParams, constrArgs) = constructorArgTypes info
      args =
        map (typeAbstraction Implicit) inductiveParams
          ++ map unnamedParameter constrArgs
  saturatedTy <- constructorReturnType c
  return (foldFunType args saturatedTy)

constructorReturnType :: Member (Reader InfoTable) r => ConstrName -> Sem r Expression
constructorReturnType c = do
  info <- lookupConstructor c
  let inductiveParams = fst (constructorArgTypes info)
      ind = ExpressionIden (IdenInductive (info ^. constructorInfoInductive))
      saturatedTy1 = foldExplicitApplication ind (map (ExpressionIden . IdenVar) inductiveParams)
      saturatedTy =
        foldl'
          ( \t v ->
              ExpressionApplication
                ( Application
                    { _appLeft = t,
                      _appRight = ExpressionIden (IdenVar v),
                      _appImplicit = Explicit
                    }
                )
          )
          ind
          inductiveParams
  unless (saturatedTy1 == saturatedTy) impossible
  return saturatedTy

getAxiomBuiltinInfo :: Member (Reader InfoTable) r => Name -> Sem r (Maybe BuiltinAxiom)
getAxiomBuiltinInfo n = do
  maybeAxiomInfo <- HashMap.lookup n <$> asks (^. infoAxioms)
  return $ case maybeAxiomInfo of
    Just axiomInfo -> axiomInfo ^. axiomInfoDef . axiomBuiltin
    Nothing -> Nothing

getFunctionBuiltinInfo :: Member (Reader InfoTable) r => Name -> Sem r (Maybe BuiltinFunction)
getFunctionBuiltinInfo n = do
  maybeFunInfo <- HashMap.lookup n <$> asks (^. infoFunctions)
  return $ case maybeFunInfo of
    Just funInfo -> funInfo ^. functionInfoDef . funDefBuiltin
    Nothing -> Nothing
