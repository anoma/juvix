module Juvix.Syntax.MicroJuvix.Reachability where

import Juvix.Prelude
import Juvix.Syntax.Abstract.NameDependencyInfo
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.MicroJuvixArityResult qualified as MicroArity
import Juvix.Syntax.MicroJuvix.MicroJuvixResult
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult qualified as MicroTyped

filterUnreachable :: MicroTyped.MicroJuvixTypedResult -> MicroTyped.MicroJuvixTypedResult
filterUnreachable r = r {MicroTyped._resultModules = modules'}
  where
    depInfo = r ^. (MicroTyped.resultMicroJuvixArityResult . MicroArity.resultMicroJuvixResult . resultDepInfo)
    modules = r ^. MicroTyped.resultModules
    modules' = run $ runReader depInfo (mapM goModule modules)

returnIfReachable :: Member (Reader NameDependencyInfo) r => Name -> a -> Sem r (Maybe a)
returnIfReachable n a = do
  depInfo <- ask
  return $ if isReachable depInfo n then Just a else Nothing

goModule :: Member (Reader NameDependencyInfo) r => Module -> Sem r Module
goModule m = do
  stmts <- mapM goStatement (body ^. moduleStatements)
  return m {_moduleBody = body {_moduleStatements = catMaybes stmts}}
  where
    body = m ^. moduleBody

goStatement :: Member (Reader NameDependencyInfo) r => Statement -> Sem r (Maybe Statement)
goStatement s = case s of
  StatementInductive i -> returnIfReachable (i ^. inductiveName) s
  StatementFunction f -> returnIfReachable (f ^. funDefName) s
  StatementForeign {} -> return (Just s)
  StatementAxiom ax -> returnIfReachable (ax ^. axiomName) s
  StatementInclude i -> do
    m <- goModule (i ^. includeModule)
    return (Just (StatementInclude i {_includeModule = m}))
