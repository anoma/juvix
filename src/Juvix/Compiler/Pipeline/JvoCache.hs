module Juvix.Compiler.Pipeline.JvoCache where

import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude

type JvoCache = Cache (Path Abs File) (Maybe Store.ModuleInfo)

evalJvoCache :: (Members '[TaggedLock, Files] r) => Sem (JvoCache ': r) a -> Sem r a
evalJvoCache = evalCacheEmpty Serialize.loadFromFile

-- | Used to fill the cache in parallel
preLoadFromJvoFile :: (Members '[JvoCache] r) => ImportNode -> Sem r ()
preLoadFromJvoFile =
  void
    . fmap force
    . cacheGetResult @(Path Abs File) @(Maybe Store.ModuleInfo)
    . (^. importNodeAbsFile)

loadFromJvoFile :: (Members '[JvoCache] r) => Path Abs File -> Sem r (Maybe Store.ModuleInfo)
loadFromJvoFile = cacheGet
