module Juvix.Compiler.Pipeline.JvoCache where

import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Extra.Serialize qualified as Serialize
import Juvix.Prelude

type JvoCache = Cache (Path Abs File) (Maybe Store.ModuleInfo)

evalJvoCache :: (Members '[TaggedLock, Files] r) => Sem (JvoCache ': r) a -> Sem r a
evalJvoCache = evalCacheEmpty Serialize.loadFromFile

loadFromFile :: (Members '[JvoCache] r) => Path Abs File -> Sem r (Maybe Store.ModuleInfo)
loadFromFile = cacheGet
