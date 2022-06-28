module MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypePropagation (collectTypeCalls) where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder

collectTypeCalls :: MicroJuvixTypedResult -> TypeCalls
collectTypeCalls res = run (execState emptyCalls (runReader typesTable (runReader infoTable goTopLevel)))
  where
    goTopLevel :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r => Sem r ()
    goTopLevel = mapM_ goConcreteFun entries
      where
        allModules :: [Module]
        allModules = reachableModules main
        -- the list of functions defined in any module with concrete types.
        entries :: [FunctionDef]
        entries =
          [ f
            | m <- allModules,
              StatementFunction f <- m ^. moduleBody . moduleStatements,
              hasConcreteType f
          ]
          where
            hasConcreteType :: FunctionDef -> Bool
            hasConcreteType = isJust . mkConcreteType . (^. funDefType)
        goConcreteFun :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r => FunctionDef -> Sem r ()
        goConcreteFun fun = do
          calls <- fmap (fmap mkConcreteType') <$> lookupTypeCalls callerFun
          mapM_ go calls
          where
            callerFun :: Caller
            callerFun = CallerFunction (fun ^. funDefName)
    main :: Module
    main = res ^. mainModule
    typesTable :: TypeCallsMap
    typesTable = buildTypeCallMap res
    infoTable :: InfoTable
    infoTable = buildTable (res ^. resultModules)

isRegistered :: Members '[State TypeCalls] r => ConcreteTypeCall -> Sem r Bool
isRegistered c = do
  t <- gets (^. typeCallSet)
  return (isJust (HashMap.lookup (c ^. typeCallIden) t >>= HashMap.lookup c))

register :: Members '[State TypeCalls] r => ConcreteTypeCall -> ConcreteSubs -> Sem r ()
register c t = modify (over typeCallSet (HashMap.alter (Just . addElem) (c ^. typeCallIden)))
  where
    addElem :: Maybe (HashMap ConcreteTypeCall ConcreteSubs) -> HashMap ConcreteTypeCall ConcreteSubs
    addElem = \case
      Nothing -> HashMap.singleton c t
      Just m -> HashMap.insert c t m

lookupTypeCalls :: Members '[Reader TypeCallsMap] r => Caller -> Sem r [TypeCall]
lookupTypeCalls t = maybe [] toList <$> asks (^. typeCallsMap . at t)

toConcreteCall :: ConcreteSubs -> TypeCall -> ConcreteTypeCall
toConcreteCall m = fmap (substitutionConcrete m)

go ::
  Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r =>
  ConcreteTypeCall ->
  Sem r ()
go c = unlessM (isRegistered c) $ do
  calls :: [TypeCall] <- lookupTypeCalls caller
  assocs :: ConcreteSubs <- case c ^. typeCallIden of
    InductiveIden i -> do
      def <- (^. inductiveInfoDef) <$> lookupInductive i
      return (inductiveTypeVarsAssoc def (c ^. typeCallArguments))
    FunctionIden f -> do
      def <- (^. functionInfoDef) <$> lookupFunction f
      return (functionTypeVarsAssoc def (c ^. typeCallArguments))
  let ccalls :: [ConcreteTypeCall]
      ccalls = fmap (toConcreteCall assocs) calls
  register c assocs
  mapM_ go ccalls
  where
    caller :: Caller
    caller = typeCallIdenToCaller (c ^. typeCallIden)
