module Juvix.Compiler.Pipeline.ModuleInfoCache where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Result
import Juvix.Compiler.Store.Language qualified as Store
import Juvix.Data.Effect.Cache
import Juvix.Prelude.Base

newtype EntryIndex = EntryIndex
  { _entryIxEntry :: EntryPoint
  }

makeLenses ''EntryIndex

instance Eq EntryIndex where
  (==) = (==) `on` (^. entryIxEntry . entryPointModulePath)

instance Hashable EntryIndex where
  hashWithSalt s = hashWithSalt s . (^. entryIxEntry . entryPointModulePath)

type ModuleInfoCache' a = Cache EntryIndex a

type ModuleInfoCache = ModuleInfoCache' (PipelineResult Store.ModuleInfo)
