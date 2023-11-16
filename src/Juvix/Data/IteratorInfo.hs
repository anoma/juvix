module Juvix.Data.IteratorInfo where

import Juvix.Extra.Serialize
import Juvix.Prelude.Base

data IteratorInfo = IteratorInfo
  { _iteratorInfoInitNum :: Maybe Int,
    _iteratorInfoRangeNum :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Serialize IteratorInfo

makeLenses ''IteratorInfo

emptyIteratorInfo :: IteratorInfo
emptyIteratorInfo =
  IteratorInfo
    { _iteratorInfoInitNum = Nothing,
      _iteratorInfoRangeNum = Nothing
    }
