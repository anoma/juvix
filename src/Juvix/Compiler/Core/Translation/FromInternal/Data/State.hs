module Juvix.Compiler.Core.Translation.FromInternal.Data.State where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language (Index)
import Juvix.Prelude

newtype SucState = SucState
  { _sucStateSucs :: Integer
  }

newtype SucTable = SucTable
  { _sucTableTable :: HashMap Text Integer
  }
  deriving stock (Show)

data LetSpec = LetSpec
  { _letSpecBinder :: Text,
    _letSpecSucs :: Integer,
    _letSpecIndex :: Index
  }

makeLenses ''SucState
makeLenses ''SucTable
makeLenses ''LetSpec

emptySucState :: SucState
emptySucState = SucState 0

emptySucTable :: SucTable
emptySucTable = SucTable HashMap.empty
