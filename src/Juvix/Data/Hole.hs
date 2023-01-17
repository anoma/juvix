module Juvix.Data.Hole where

import Juvix.Data.Loc
import Juvix.Data.NameId
import Juvix.Prelude.Base
import Prettyprinter

data Hole = Hole
  { _holeId :: NameId,
    _holeLoc :: Interval
  }
  deriving stock (Show, Data)

makeLenses ''Hole

instance Eq Hole where
  (==) = (==) `on` (^. holeId)

instance Ord Hole where
  compare = compare `on` (^. holeId)

instance Hashable Hole where
  hashWithSalt s = hashWithSalt s . (^. holeId)

instance HasLoc Hole where
  getLoc = (^. holeLoc)

instance Pretty Hole where
  pretty = const "_"
