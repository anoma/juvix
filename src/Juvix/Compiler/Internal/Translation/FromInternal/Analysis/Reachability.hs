module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability where

import Juvix.Compiler.Abstract.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Prelude

filterUnreachable :: Typed.InternalTypedResult -> Typed.InternalTypedResult
filterUnreachable r = r {Typed._resultModules = modules'}
  where
    depInfo = r ^. (Typed.resultInternalArityResult . Arity.resultInternalResult . resultDepInfo)
    modules = r ^. Typed.resultModules
    modules' = run $ runReader depInfo (mapM goModule modules)

askIsReachable :: Member (Reader NameDependencyInfo) r => Name -> Sem r Bool
askIsReachable n = do
  depInfo <- ask
  return (isReachable depInfo n)

returnIfReachable :: (Member (Reader NameDependencyInfo) r) => Name -> a -> Sem r (Maybe a)
returnIfReachable n a = do
  r <- askIsReachable n
  return
    if
        | r -> Just a
        | otherwise -> Nothing

goModule :: (Member (Reader NameDependencyInfo) r) => Module -> Sem r Module
goModule m = do
  stmts <- mapM goStatement (body ^. moduleStatements)
  return m {_moduleBody = body {_moduleStatements = catMaybes stmts}}
  where
    body = m ^. moduleBody

goStatement :: forall r. Member (Reader NameDependencyInfo) r => Statement -> Sem r (Maybe Statement)
goStatement s = case s of
  StatementMutual m -> fmap StatementMutual <$> goMutual m
  StatementAxiom ax -> returnIfReachable (ax ^. axiomName) s
  StatementInclude i -> do
    m <- goModule (i ^. includeModule)
    return (Just (StatementInclude i {_includeModule = m}))
  where
    -- note that the first mutual statement is reachable iff all are reachable
    goMutual :: MutualBlock -> Sem r (Maybe MutualBlock)
    goMutual b@(MutualBlock (m :| _)) = case m of
      StatementFunction f -> returnIfReachable (f ^. funDefName) b
      StatementInductive f -> returnIfReachable (f ^. inductiveName) b
