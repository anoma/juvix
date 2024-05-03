module Juvix.Compiler.Pipeline.Loader.PathResolver.Data where

import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude

data ResolverEnv = ResolverEnv
  { -- | The root path of the current project being resolved
    _envRoot :: Path Abs Dir,
    -- | The root path of the initial project (i.e the first project considered in the resolution)
    _envInitialRoot :: Path Abs Dir,
    -- | The path of the input file *if* we are using the global project
    _envSingleFile :: Maybe (Path Abs File),
    _envLockfileInfo :: Maybe LockfileInfo
  }

data ResolverCacheItem = ResolverCacheItem
  { _resolverCacheItemPackage :: PackageInfo,
    _resolverCacheItemDependency :: LockfileDependency
  }
  deriving stock (Show)

data ResolverState = ResolverState
  { -- | juvix files indexed by relative path
    _resolverFiles :: HashMap (Path Rel File) (NonEmpty PackageInfo),
    -- | PackageInfos indexed by root
    _resolverCache :: HashMap (Path Abs Dir) ResolverCacheItem,
    _resolverHasRemoteDependencies :: Bool,
    _resolverShouldUpdateLockfile :: Bool
  }
  deriving stock (Show)

data ResolvedDependency = ResolvedDependency
  { _resolvedDependencyPath :: Path Abs Dir,
    _resolvedDependencyDependency :: Dependency
  }

makeLenses ''ResolverState
makeLenses ''ResolverEnv
makeLenses ''ResolvedDependency
makeLenses ''ResolverCacheItem

iniResolverState :: ResolverState
iniResolverState =
  ResolverState
    { _resolverCache = mempty,
      _resolverFiles = mempty,
      _resolverHasRemoteDependencies = False,
      _resolverShouldUpdateLockfile = False
    }

setHasRemoteDependencies :: (Member (State ResolverState) r) => Sem r ()
setHasRemoteDependencies = modify' (set resolverHasRemoteDependencies True)

setShouldUpdateLockfile :: (Member (State ResolverState) r) => Sem r ()
setShouldUpdateLockfile = modify' (set resolverShouldUpdateLockfile True)

checkRemoteDependency :: (Member (State ResolverState) r) => ResolvedDependency -> Sem r ()
checkRemoteDependency d = case d ^. resolvedDependencyDependency of
  DependencyGit {} -> setHasRemoteDependencies
  DependencyPath {} -> return ()

withEnvRoot :: (Members '[Reader ResolverEnv] r) => Path Abs Dir -> Sem r a -> Sem r a
withEnvRoot root' = local (set envRoot root')

withEnvInitialRoot :: (Members '[Reader ResolverEnv] r) => Path Abs Dir -> Sem r a -> Sem r a
withEnvInitialRoot projectRoot = local (set envInitialRoot projectRoot) . local (set envRoot projectRoot)

withLockfile :: (Members '[Reader ResolverEnv] r) => LockfileInfo -> Sem r a -> Sem r a
withLockfile f = local (set envLockfileInfo (Just f))

setResolverCacheItem :: (Members '[Files, State ResolverState] r) => Path Abs Dir -> Maybe (ResolverCacheItem) -> Sem r ()
setResolverCacheItem p mi = do
  np <- normalizeDir p
  modify' (set (resolverCache . at np) mi)

getResolverCacheItem :: (Members '[Files, State ResolverState] r) => Path Abs Dir -> Sem r (Maybe (ResolverCacheItem))
getResolverCacheItem p = do
  np <- normalizeDir p
  gets (^. resolverCache . at np)
