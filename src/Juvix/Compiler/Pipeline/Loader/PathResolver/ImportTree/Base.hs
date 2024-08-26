module Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode,
    ImportTree,
    importTree,
    importTreeReverse,
    importTreeEdges,
    importTreeNodes,
    importTreeProjectNodes,
    importTreeNodesByPackage,
    importTreeSize,
    ImportTreeBuilder,
    runImportTreeBuilder,
    ignoreImportTreeBuilder,
    execImportTreeBuilder,
    withImportNode,
    importTreeAddEdge,
    ImportTreeNodeBuilder,
    mkImportTreeStats,
    ImportTreeStats (..),
    importTreeStatsTotalModules,
    importTreeStatsTotalEdges,
    importTreeStatsHeight,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Prelude

data ImportTreeStats = ImportTreeStats
  { _importTreeStatsTotalModules :: Int,
    _importTreeStatsTotalEdges :: Int,
    _importTreeStatsHeight :: Int
  }

data ImportTree = ImportTree
  { -- | A ∈ importTree[B] ⇔ B imports A. Every scanned node is a key, even if
    -- it has no imports.
    _fimportTree :: HashMap ImportNode (HashSet ImportNode),
    -- | A ∈ importTreeSym[B] ⇔ A imports B. Every scanned node is a key, even
    -- if it not imported by another node.
    _fimportTreeReverse :: HashMap ImportNode (HashSet ImportNode),
    -- | Useful for reporting a concrete error in case of a cycle.
    _fimportTreeEdges :: HashMap ImportNode (HashSet ImportScan)
  }

data ImportTreeBuilder :: Effect where
  InternalRegisterNode :: ImportNode -> ImportTreeBuilder m ()
  InternalRegisterEdge :: ImportScan -> ImportNode -> ImportNode -> ImportTreeBuilder m ()

data ImportTreeNodeBuilder :: Effect where
  ImportTreeAddEdge :: ImportScan -> ImportNode -> ImportTreeNodeBuilder m ()

makeLenses ''ImportTree
makeLenses ''ImportTreeStats
makeSem ''ImportTreeBuilder
makeSem ''ImportTreeNodeBuilder

emptyImportTree :: ImportTree
emptyImportTree =
  ImportTree
    { _fimportTree = mempty,
      _fimportTreeReverse = mempty,
      _fimportTreeEdges = mempty
    }

ignoreImportTreeBuilder :: Sem (ImportTreeBuilder ': r) a -> Sem r a
ignoreImportTreeBuilder = fmap snd . runImportTreeBuilder

execImportTreeBuilder :: Sem (ImportTreeBuilder ': r) a -> Sem r (ImportTree)
execImportTreeBuilder = fmap fst . runImportTreeBuilder

runImportTreeBuilder :: Sem (ImportTreeBuilder ': r) a -> Sem r (ImportTree, a)
runImportTreeBuilder = reinterpret (runState emptyImportTree) $ \case
  InternalRegisterNode n -> modify (over fimportTree (hashMapInsertWeak n mempty))
  InternalRegisterEdge importScan fromNode toNode -> do
    modify (over fimportTree (insertHelper fromNode toNode))
    modify (over fimportTreeReverse (insertHelper toNode fromNode))
    modify (over fimportTreeEdges (insertHelper fromNode importScan))
    where

  where
    insertHelper :: (Hashable k, Hashable v) => k -> v -> HashMap k (HashSet v) -> HashMap k (HashSet v)
    insertHelper k v = over (at k) (Just . maybe (HashSet.singleton v) (HashSet.insert v))

withImportNode :: (Members '[ImportTreeBuilder] r) => ImportNode -> Sem (ImportTreeNodeBuilder ': r) a -> Sem r a
withImportNode fromNode m = do
  internalRegisterNode fromNode
  (`interpret` m) $ \case
    ImportTreeAddEdge importScan toNode -> internalRegisterEdge importScan fromNode toNode

importTreeNodesByPackage :: ImportTree -> HashMap (Path Abs Dir) (HashSet ImportNode)
importTreeNodesByPackage tree = run . execState mempty $
  forM_ (tree ^. importTreeNodes) $ \node ->
    modify @(HashMap (Path Abs Dir) (HashSet ImportNode))
      (over (at (node ^. importNodePackageRoot)) (Just . maybe (HashSet.singleton node) (HashSet.insert node)))

importTree :: SimpleGetter ImportTree (HashMap ImportNode (HashSet ImportNode))
importTree = fimportTree

importTreeReverse :: SimpleGetter ImportTree (HashMap ImportNode (HashSet ImportNode))
importTreeReverse = fimportTreeReverse

importTreeNodes :: SimpleGetter ImportTree (HashSet ImportNode)
importTreeNodes = importTree . to HashMap.keysSet

importTreeProjectNodes :: Path Abs Dir -> ImportTree -> [ImportNode]
importTreeProjectNodes pkgRoot tree = mapMaybe projectFile (toList (tree ^. importTreeNodes))
  where
    projectFile :: ImportNode -> Maybe ImportNode
    projectFile i = do
      guard (i ^. importNodePackageRoot == pkgRoot)
      return i

importTreeEdges :: SimpleGetter ImportTree (HashMap ImportNode (HashSet ImportScan))
importTreeEdges = fimportTreeEdges

-- | Returns the number of modules
importTreeSize :: (Integral n) => ImportTree -> n
importTreeSize = fromIntegral . length . (^. importTreeNodes)

-- | The import tree is assumed to have no cycles
mkImportTreeStats :: ImportTree -> ImportTreeStats
mkImportTreeStats ImportTree {..} =
  ImportTreeStats
    { _importTreeStatsTotalModules = length nodes,
      _importTreeStatsTotalEdges = sum . map length . toList $ _fimportTree,
      _importTreeStatsHeight = maximum nodesHeight
    }
  where
    nodes :: [ImportNode]
    nodes = HashMap.keys _fimportTree

    nodesHeight :: LazyHashMap ImportNode Int
    nodesHeight = lazyHashMap [(n, computeHeight n) | n <- nodes]
      where
        computeHeight :: ImportNode -> Int
        computeHeight n = case nonEmpty (_fimportTree ^. at n . _Just) of
          Nothing -> 0
          Just l -> 1 + maximum1 (getHeight <$> l)
          where
            getHeight :: ImportNode -> Int
            getHeight m = nodesHeight ^?! at m . _Just
