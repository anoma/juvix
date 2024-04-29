module Juvix.Compiler.Pipeline.Loader.PathResolver.Data where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Lockfile
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Prelude
import Juvix.Prelude.Pretty

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
    _resolverHasRemoteDependencies :: Bool,
    _resolverShouldUpdateLockfile :: Bool
  }
  deriving stock (Show)

data ResolvedDependency = ResolvedDependency
  { _resolvedDependencyPath :: Path Abs Dir,
    _resolvedDependencyDependency :: Dependency
  }

data ImportNode = ImportNode
  { _importNodePackageRoot :: Path Abs Dir,
    _importNodeFile :: Path Rel File
  }
  deriving stock (Eq, Ord, Generic, Show)

instance Pretty ImportNode where
  pretty ImportNode {..} = pretty _importNodePackageRoot <+> ":" <+> show _importNodeFile

instance Hashable ImportNode

data ImportTreeStats = ImportTreeStats
  { _importTreeStatsTotalModules :: Int,
    _importTreeStatsTotalEdges :: Int,
    _importTreeStatsHeight :: Int
  }

data ImportTree = ImportTree
  { -- | A ∈ importTree[B] ⇔ B imports A. Every scanned node is a key, even if
    -- it has no imports.
    _importTree :: HashMap ImportNode (HashSet ImportNode),
    -- | A ∈ importTreeSym[B] ⇔ A imports B. Every scanned node is a key, even
    -- if it not imported by another node.
    _importTreeReverse :: HashMap ImportNode (HashSet ImportNode)
  }

emptyImportTree :: [ImportNode] -> ImportTree
emptyImportTree nodes =
  ImportTree
    { _importTree = hashMap [(n, mempty) | n <- nodes],
      _importTreeReverse = hashMap [(n, mempty) | n <- nodes]
    }

makeLenses ''ImportTree
makeLenses ''ImportTreeStats
makeLenses ''ImportNode
makeLenses ''ResolverState
makeLenses ''ResolverEnv
makeLenses ''ResolvedDependency
makeLenses ''ResolverCacheItem

allPackageInfos :: ResolverState -> HashMap (Path Abs Dir) PackageInfo
allPackageInfos = fmap (^. resolverCacheItemPackage) . (^. resolverCache)

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

-- | The import tree is assumed to have no cycles
mkImportTreeStats :: ImportTree -> ImportTreeStats
mkImportTreeStats ImportTree {..} =
    ImportTreeStats
      { _importTreeStatsTotalModules = length nodes,
        _importTreeStatsTotalEdges = sum . map length . toList $ _importTree,
        _importTreeStatsHeight = maximum nodesHeight
      }
  where
    nodes :: [ImportNode]
    nodes = HashMap.keys _importTree

    nodesHeight :: LazyHashMap ImportNode Int
    nodesHeight = lazyHashMap [(n, computeHeight n) | n <- nodes]
      where
        computeHeight :: ImportNode -> Int
        computeHeight n = case nonEmpty (_importTree ^. at n . _Just) of
          Nothing -> 0
          Just l -> 1 + maximum1 (getHeight <$> l)
          where
            getHeight :: ImportNode -> Int
            getHeight m = nodesHeight ^?! at m . _Just
