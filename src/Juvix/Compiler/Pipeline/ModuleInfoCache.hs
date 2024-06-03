module Juvix.Compiler.Pipeline.ModuleInfoCache where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.Cache
import Juvix.Prelude

data EntryIndex = EntryIndex
  { _entryIxEntry :: EntryPoint,
    _entryIxResolverRoot :: Path Abs Dir
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

mkEntryIndex :: (Members '[Reader EntryPoint] r) => Path Abs Dir -> Path Abs File -> Sem r EntryIndex
mkEntryIndex _entryIxResolverRoot path = do
  entry <- ask
  let stdin'
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
        _entryIxResolverRoot
      }
