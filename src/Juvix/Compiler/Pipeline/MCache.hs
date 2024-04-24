module Juvix.Compiler.Pipeline.MCache where

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

type MCache' a = Cache EntryIndex a

type MCache = MCache' (PipelineResult Store.ModuleInfo)
