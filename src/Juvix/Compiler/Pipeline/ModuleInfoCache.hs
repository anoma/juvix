module Juvix.Compiler.Pipeline.ModuleInfoCache where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.Cache
import Juvix.Prelude

data EntryIndex = EntryIndex
  { _entryIxEntry :: EntryPoint,
    _entryIxImportNode :: ImportNode
  }

makeLenses ''EntryIndex

instance Eq EntryIndex where
  (==) = (==) `on` (^. entryIxEntry . entryPointModulePath)

instance Hashable EntryIndex where
  hashWithSalt s = hashWithSalt s . (^. entryIxEntry . entryPointModulePath)

type ModuleInfoCache = Cache EntryIndex (PipelineResult Store.ModuleInfo)

entryIndexPath :: EntryIndex -> Path Abs File
entryIndexPath = fromMaybe err . (^. entryIxEntry . entryPointModulePath)
  where
    err :: a
    err = error "unexpected: EntryIndex should always have a path"

mkEntryIndex :: (Members '[Reader EntryPoint] r) => ImportNode -> Sem r EntryIndex
mkEntryIndex node = do
  entry <- ask
  let path = node ^. importNodeAbsFile
      stdin'
        | Just path == entry ^. entryPointModulePath = entry ^. entryPointStdin
        | otherwise = Nothing
      entry' =
        entry
          { _entryPointStdin = stdin',
            _entryPointModulePath = Just path
          }
  return
    EntryIndex
      { _entryIxEntry = entry',
        _entryIxImportNode = node
      }
