module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Reachability where

import Juvix.Compiler.Internal.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking qualified as Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Typed
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

type MCache = Cache ModuleIndex Module

filterUnreachable :: Members '[Reader EntryPoint] r => Typed.InternalTypedResult -> Sem r Typed.InternalTypedResult
filterUnreachable r = do
  asks (^. entryPointSymbolPruningMode) >>= \case
    KeepAll -> return r
    FilterUnreachable -> return (set Typed.resultModules modules' r)
  where
    depInfo = r ^. (Typed.resultInternalArityResult . Arity.resultInternalResult . resultDepInfo)
    modules = r ^. Typed.resultModules
    modules' =
      run
        . runReader depInfo
        . evalCacheEmpty goModuleNoCache
        $ mapM goModule modules

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

goModuleNoCache :: Members [Reader NameDependencyInfo, MCache] r => ModuleIndex -> Sem r Module
goModuleNoCache (ModuleIndex m) = do
  stmts <- mapM goStatement (body ^. moduleStatements)
  let body' = set moduleStatements (catMaybes stmts) body
  return (set moduleBody body' m)
  where
    body = m ^. moduleBody

goModule :: Members [Reader NameDependencyInfo, MCache] r => Module -> Sem r Module
goModule = cacheGet . ModuleIndex

goModuleIndex :: Members [Reader NameDependencyInfo, MCache] r => ModuleIndex -> Sem r ModuleIndex
goModuleIndex = fmap ModuleIndex . cacheGet

goStatement :: forall r. Member (Reader NameDependencyInfo) r => Statement -> Sem r (Maybe Statement)
goStatement s = case s of
  StatementMutual m -> fmap StatementMutual <$> goMutual m
  StatementAxiom ax -> returnIfReachable (ax ^. axiomName) s
  where
    -- note that the first mutual statement is reachable iff all are reachable
    goMutual :: MutualBlock -> Sem r (Maybe MutualBlock)
    goMutual b@(MutualBlock (m :| _)) = case m of
      StatementFunction f -> returnIfReachable (f ^. funDefName) b
      StatementInductive f -> returnIfReachable (f ^. inductiveName) b

goInclude :: forall r. Members [Reader NameDependencyInfo, MCache] r => Import -> Sem r Import
goInclude i = do
  _importModule <- goModuleIndex (i ^. importModule)
  return Import {..}
