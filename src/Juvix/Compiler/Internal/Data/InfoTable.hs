module Juvix.Compiler.Internal.Data.InfoTable
  ( module Juvix.Compiler.Internal.Data.InfoTable,
    module Juvix.Compiler.Internal.Data.InfoTable.Base,
  )
where

import Data.Generics.Uniplate.Data
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InfoTable.Base
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty (ppTrace)
import Juvix.Prelude

buildTable :: Foldable f => f Module -> InfoTable
buildTable = run . evalState (mempty :: Cache) . buildTable'

buildTable1 :: Module -> InfoTable
buildTable1 = run . evalState (mempty :: Cache) . buildTable1'

buildTable' :: (Members '[State Cache] r, Foldable f) => f Module -> Sem r InfoTable
buildTable' = mconcatMap buildTable1'

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

-- | moduleName ↦ infoTable
type Cache = HashMap Name InfoTable

buildTable1' :: forall r. (Members '[State Cache] r) => Module -> Sem r InfoTable
buildTable1' m = do
  mi <- gets @Cache (^. at (m ^. moduleName))
  maybe compute return mi
  where
    compute :: Sem r InfoTable
    compute = do
      infoInc <- buildTable' (map (^. includeModule) includes)
      return (InfoTable {..} <> infoInc)

    includes :: [Include]
    includes = [i | StatementInclude i <- ss]

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
               | s <- filter (not . isInclude) ss,
                 f <- letFunctionDefs s
             ]
      where
        isInclude :: Statement -> Bool
        isInclude = \case
          StatementInclude {} -> True
          _ -> False

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
