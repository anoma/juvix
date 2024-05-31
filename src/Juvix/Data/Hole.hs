module Juvix.Data.Hole where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All (kwWildcard)
import Juvix.Data.Loc
import Juvix.Data.NameId
import Juvix.Extra.Serialize as S
import Juvix.Prelude.Base
import Prettyprinter

data Hole = Hole
  { _holeId :: NameId,
    _holeKw :: KeywordRef
  }
  deriving stock (Show, Data, Generic)

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

instance NFData Hole

instance Serialize Hole where
  put Hole {..} = do
    S.put _holeId
    S.put (_holeKw ^. keywordRefInterval)

  get = do
    i <- S.get
    loc <- S.get
    return $ mkHole loc i

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
