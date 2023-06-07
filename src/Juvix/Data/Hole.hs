module Juvix.Data.Hole where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All (kwWildcard)
import Juvix.Data.Loc
import Juvix.Data.NameId
import Juvix.Prelude.Base
import Prettyprinter

data Hole = Hole
  { _holeId :: NameId,
    _holeKw :: KeywordRef
  }
  deriving stock (Show, Data)

mkHole :: Interval -> NameId -> Hole
mkHole loc uid =
  Hole
    { _holeId = uid,
      _holeKw = r
    }
  where
    r =
      KeywordRef
        { _keywordRefKeyword = kwWildcard,
          _keywordRefInterval = loc,
          _keywordRefUnicode = Ascii
        }

makeLenses ''Hole

instance Eq Hole where
  (==) = (==) `on` (^. holeId)

instance Ord Hole where
  compare = compare `on` (^. holeId)

instance Hashable Hole where
  hashWithSalt s = hashWithSalt s . (^. holeId)

instance HasLoc Hole where
  getLoc = getLoc . (^. holeKw)

instance Pretty Hole where
  pretty = const "_"
