module Juvix.Data.IteratorInfo where

import Juvix.Prelude.Base

data IteratorInfo = IteratorInfo
  { _iteratorInfoInitNum :: Maybe Int,
    _iteratorInfoRangeNum :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic)

makeLenses ''IteratorInfo
