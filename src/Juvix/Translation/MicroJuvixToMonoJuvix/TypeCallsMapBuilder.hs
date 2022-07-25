module Juvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder (buildTypeCallMap) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language.Extra
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult

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

goAxiomDef :: Members '[State TypeCallsMap, Reader InfoTable] r => AxiomDef -> Sem r ()
goAxiomDef a =
  runReader
    (CallerAxiom (a ^. axiomName))
    (goExpression (a ^. axiomType))

goFunctionDef :: Members '[State TypeCallsMap, Reader InfoTable] r => FunctionDef -> Sem r ()
goFunctionDef d = runReader (CallerFunction (d ^. funDefName)) $ do
  goExpression (d ^. funDefType)
  mapM_ goFunctionClause (d ^. funDefClauses)

goFunctionClause :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => FunctionClause -> Sem r ()
goFunctionClause c = goExpression (c ^. clauseBody)

goInductiveDef :: Members '[State TypeCallsMap, Reader InfoTable] r => InductiveDef -> Sem r ()
goInductiveDef d = runReader (CallerInductive (d ^. inductiveName)) $ do
  mapM_ goInductiveParameter (d ^. inductiveParameters)
  mapM_ goInductiveConstructorDef (d ^. inductiveConstructors)

goInductiveParameter :: InductiveParameter -> Sem r ()
goInductiveParameter _ = return ()

goInductiveConstructorDef :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => InductiveConstructorDef -> Sem r ()
goInductiveConstructorDef c = mapM_ goExpression (c ^. inductiveConstructorParameters)

goParam :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => FunctionParameter -> Sem r ()
goParam (FunctionParameter _ _ ty) = goExpression ty

goFunction :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => Function -> Sem r ()
goFunction (Function l r) = do
  goParam l
  goExpression r

registerTypeCall :: Members '[State TypeCallsMap] r => Caller -> TypeCall -> Sem r ()
registerTypeCall caller t = modify (over typeCallsMap addElem)
  where
    addElem :: HashMap Caller (HashSet TypeCall) -> HashMap Caller (HashSet TypeCall)
    addElem = HashMap.alter (Just . aux) caller
      where
        aux = \case
          Nothing -> HashSet.singleton t
          Just l -> HashSet.insert t l

goApplication :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => Application -> Sem r ()
goApplication a = do
  let (f, args) = unfoldApplication a
  mapM_ goExpression args
  case f of
    ExpressionIden i -> case i of
      (IdenFunction fun) -> do
        funTy <- (^. functionInfoDef . funDefType) <$> lookupFunction fun
        let numTyArgs = length (fst (unfoldTypeAbsType funTy))
        when (numTyArgs > 0) $ do
          let tyArgs = take' numTyArgs args
          caller <- ask
          registerTypeCall
            caller
            TypeCall'
              { _typeCallIden = FunctionIden fun,
                _typeCallArguments = tyArgs
              }
      (IdenInductive ind) -> do
        caller <- ask
        registerTypeCall
          caller
          TypeCall'
            { _typeCallIden = InductiveIden ind,
              _typeCallArguments = args
            }
      _ -> return ()
    -- Note: cosntructors do not need to be checked as they are already covered
    -- by inspecting the types
    _ -> return ()
  where
    take' :: Int -> NonEmpty a -> NonEmpty a
    take' n l
      | 0 < n = fromMaybe impossible . nonEmpty . NonEmpty.take n $ l
      | otherwise = error ("take' non-positive: " <> show n)

goExpression :: Members '[State TypeCallsMap, Reader Caller, Reader InfoTable] r => Expression -> Sem r ()
goExpression = \case
  ExpressionIden {} -> return ()
  ExpressionUniverse {} -> return ()
  ExpressionApplication a -> goApplication a
  ExpressionFunction a -> goFunction a
  ExpressionLiteral {} -> return ()
  ExpressionHole {} -> impossible
  ExpressionLambda {} -> impossible
