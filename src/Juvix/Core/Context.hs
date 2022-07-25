module Juvix.Core.Context where

import Juvix.Prelude
import Juvix.Core.GNode
import Juvix.Core.Type

type IdentContext i = HashMap Symbol (GNode i)

data Context i = Context
  {
    _identContext :: IdentContext i,
    _identInfo :: IdentInfo,
    _inductiveInfo :: i
  }

data IdentInfo = IdentInfo
  {
    _identName :: Text,
    _identSymbol :: Symbol,
    _identType :: CoreType,
    _identArgsNum :: Int,
    _identArgsInfo :: [ArgumentInfo]
  }

data ArgumentInfo = ArgumentInfo
  {
    _argName :: Text,
    _argType :: CoreType,
    _argIsImplicit :: Bool
  }

makeLenses ''Context
