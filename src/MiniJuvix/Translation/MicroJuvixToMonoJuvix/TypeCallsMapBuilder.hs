module MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder (buildTypeCallMap) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult

buildTypeCallMap :: MicroJuvixTypedResult -> TypeCallsMap
buildTypeCallMap r =
  run
    . runReader (buildTable modules)
    . execState (TypeCallsMap mempty)
    . mapM_ goModule
    $ modules
  where
    modules = r ^. resultModules

goModule :: Members '[State TypeCallsMap, Reader InfoTable] r => Module -> Sem r ()
goModule = goModuleBody . (^. moduleBody)

goModuleBody :: Members '[State TypeCallsMap, Reader InfoTable] r => ModuleBody -> Sem r ()
goModuleBody = mapM_ goStatement . (^. moduleStatements)

goStatement :: Members '[State TypeCallsMap, Reader InfoTable] r => Statement -> Sem r ()
goStatement = \case
  StatementInductive d -> goInductiveDef d
  StatementFunction f -> goFunctionDef f
  StatementForeign {} -> return ()
  StatementInclude i -> goInclude i
  StatementAxiom a -> goAxiomDef a

goInclude :: Members '[State TypeCallsMap, Reader InfoTable] r => Include -> Sem r ()
goInclude i = goModule (i ^. includeModule)

goAxiomDef :: Members '[State TypeCallsMap] r => AxiomDef -> Sem r ()
goAxiomDef a =
  runReader
    (CallerAxiom (a ^. axiomName))
    (goType (a ^. axiomType))

goFunctionDef :: Members '[State TypeCallsMap, Reader InfoTable] r => FunctionDef -> Sem r ()
goFunctionDef d = runReader (CallerFunction (d ^. funDefName)) $ do
  goType (d ^. funDefType)
  mapM_ goFunctionClause (d ^. funDefClauses)

goFunctionClause :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => FunctionClause -> Sem r ()
goFunctionClause c = goExpression (c ^. clauseBody)

goInductiveDef :: Members '[State TypeCallsMap] r => InductiveDef -> Sem r ()
goInductiveDef d = runReader (CallerInductive (d ^. inductiveName)) $ do
  mapM_ goInductiveParameter (d ^. inductiveParameters)
  mapM_ goInductiveConstructorDef (d ^. inductiveConstructors)

goInductiveParameter :: InductiveParameter -> Sem r ()
goInductiveParameter _ = return ()

goInductiveConstructorDef :: Members '[State TypeCallsMap, Reader Caller] r => InductiveConstructorDef -> Sem r ()
goInductiveConstructorDef c = mapM_ goType (c ^. constructorParameters)

goFunction :: Members '[State TypeCallsMap, Reader Caller] r => Function -> Sem r ()
goFunction (Function l r) = do
  goType l
  goType r

registerTypeCall :: Members '[State TypeCallsMap] r => Caller -> TypeCall -> Sem r ()
registerTypeCall caller t = modify (over typeCallsMap addElem)
  where
    addElem :: HashMap Caller (HashSet TypeCall) -> HashMap Caller (HashSet TypeCall)
    addElem = HashMap.alter (Just . aux) caller
      where
        aux = \case
          Nothing -> HashSet.singleton t
          Just l -> HashSet.insert t l

goTypeApplication :: Members '[State TypeCallsMap, Reader Caller] r => TypeApplication -> Sem r ()
goTypeApplication a = do
  let (t, args) = unfoldTypeApplication a
  mapM_ goType args
  case t of
    TypeIden (TypeIdenInductive n) -> do
      caller <- ask
      registerTypeCall
        caller
        TypeCall'
          { _typeCallIden = InductiveIden n,
            _typeCallArguments = args
          }
    _ -> return ()

goTypeAbstraction :: Members '[State TypeCallsMap, Reader Caller] r => TypeAbstraction -> Sem r ()
goTypeAbstraction t = goType (t ^. typeAbsBody)

goType :: Members '[State TypeCallsMap, Reader Caller] r => Type -> Sem r ()
goType = \case
  TypeIden {} -> return ()
  TypeApp a -> goTypeApplication a
  TypeAny -> return ()
  TypeUniverse -> return ()
  TypeFunction f -> goFunction f
  TypeAbs a -> goTypeAbstraction a

goFunctionExpression ::
  Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r =>
  FunctionExpression ->
  Sem r ()
goFunctionExpression (FunctionExpression l r) = do
  goExpression l
  goExpression r

goExpression :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => Expression -> Sem r ()
goExpression = \case
  ExpressionIden {} -> return ()
  ExpressionApplication a -> goApplication a
  ExpressionFunction a -> goFunctionExpression a
  ExpressionLiteral {} -> return ()
  ExpressionTyped t -> do
    goType (t ^. typedType)
    goExpression (t ^. typedExpression)

goApplication :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => Application -> Sem r ()
goApplication a = do
  let (f, args) = unfoldApplication a
  mapM_ goExpression args
  case f of
    ExpressionIden (IdenFunction fun) -> do
      funTy <- (^. functionInfoDef . funDefType) <$> lookupFunction fun
      let numTyArgs = length (fst (unfoldTypeAbsType funTy))
      when (numTyArgs > 0) $ do
        let tyArgs = fmap expressionAsType' (take' numTyArgs args)
        caller <- ask
        registerTypeCall
          caller
          TypeCall'
            { _typeCallIden = FunctionIden fun,
              _typeCallArguments = tyArgs
            }
    _ -> return ()
  where
    take' :: Int -> NonEmpty a -> NonEmpty a
    take' n l
      | 0 < n = fromMaybe impossible . nonEmpty . NonEmpty.take n $ l
      | otherwise = error ("take' non-positive: " <> show n)
