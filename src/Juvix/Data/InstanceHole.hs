module Juvix.Data.InstanceHole where

import Juvix.Data.Hole qualified as Hole
import Juvix.Data.Keyword
import Juvix.Data.Keyword.All (kwWildcard)
import Juvix.Data.Loc
import Juvix.Data.NameId
import Juvix.Extra.Serialize
import Juvix.Prelude.Base
import Prettyprinter

fromHole :: Hole.Hole -> InstanceHole
fromHole (Hole.Hole a b) = InstanceHole a b

data InstanceHole = InstanceHole
  { _iholeId :: NameId,
    _iholeKw :: KeywordRef
  }
  deriving stock (Show, Data, Generic)

instance Serialize InstanceHole

instance NFData InstanceHole

mkInstanceHole :: Interval -> NameId -> InstanceHole
mkInstanceHole loc uid =
  InstanceHole
    { _iholeId = uid,
      _iholeKw = r
    }
  where
    r =
      KeywordRef
        { _keywordRefKeyword = kwWildcard,
          _keywordRefInterval = loc,
          _keywordRefUnicode = Ascii
        }

makeLenses ''InstanceHole

instance Eq InstanceHole where
  (==) = (==) `on` (^. iholeId)

instance Ord InstanceHole where
  compare = compare `on` (^. iholeId)

instance Hashable InstanceHole where
  hashWithSalt s = hashWithSalt s . (^. iholeId)

instance HasLoc InstanceHole where
  getLoc = getLoc . (^. iholeKw)

instance Pretty InstanceHole where
  pretty = const "_"
