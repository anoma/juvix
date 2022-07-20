module Juvix.Syntax.MicroJuvix.Reachability where

import Juvix.DependencyInfo qualified as DepInfo
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.MicroJuvixArityResult qualified as MicroArity
import Juvix.Syntax.MicroJuvix.MicroJuvixResult
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as MicroTyped

filterUnreachable :: MicroTyped.MicroJuvixTypedResult -> MicroTyped.MicroJuvixTypedResult
filterUnreachable r = r {MicroTyped._resultModules = modules'}
  where
    depInfo =
      DepInfo.computeReachability
        (r ^. (MicroTyped.resultMicroJuvixArityResult . MicroArity.resultMicroJuvixResult . resultDepInfo))
        startNames
    startNames = getTopLevelNames (head modules)
    modules = r ^. MicroTyped.resultModules
    modules' = run $ runReader depInfo (mapM goModule modules)

getTopLevelNames :: Module -> [Name]
getTopLevelNames m = mapMaybe getDeclName (m ^. (moduleBody . moduleStatements))
  where
    getDeclName :: Statement -> Maybe Name
    getDeclName = \case
      StatementInductive i -> Just (i ^. inductiveName)
      StatementFunction f -> Just (f ^. funDefName)
      StatementForeign {} -> Nothing
      StatementAxiom ax -> Just (ax ^. axiomName)
      StatementInclude {} -> Nothing

returnIfReachable :: Member (Reader DependencyInfo) r => Name -> a -> Sem r (Maybe a)
returnIfReachable n a = do
  depInfo <- ask
  return $ if DepInfo.isReachable depInfo n then Just a else Nothing

goModule :: Member (Reader DependencyInfo) r => Module -> Sem r Module
goModule m = do
  stmts <- mapM goStatement (body ^. moduleStatements)
  return m {_moduleBody = body {_moduleStatements = catMaybes stmts}}
  where
    body = m ^. moduleBody

goStatement :: Member (Reader DependencyInfo) r => Statement -> Sem r (Maybe Statement)
goStatement s = case s of
  StatementInductive i -> returnIfReachable (i ^. inductiveName) s
  StatementFunction f -> returnIfReachable (f ^. funDefName) s
  StatementForeign {} -> return (Just s)
  StatementAxiom ax -> returnIfReachable (ax ^. axiomName) s
  StatementInclude i -> do
    m <- goModule (i ^. includeModule)
    return (Just (StatementInclude i {_includeModule = m}))
