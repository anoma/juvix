module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Data where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude

data ResolverEnv = ResolverEnv
  { _envRoot :: Path Abs Dir,
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
    _resolverShouldWriteLockfile :: Bool
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
      _resolverShouldWriteLockfile = False
    }

checkShouldWriteLockfile :: (Member (State ResolverState) r) => ResolvedDependency -> Sem r ()
checkShouldWriteLockfile d = case d ^. resolvedDependencyDependency of
  DependencyGit {} -> modify' (set resolverShouldWriteLockfile True)
  DependencyPath {} -> return ()

withEnvRoot :: (Members '[Reader ResolverEnv] r) => Path Abs Dir -> Sem r a -> Sem r a
withEnvRoot root' = local (set envRoot root')

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
