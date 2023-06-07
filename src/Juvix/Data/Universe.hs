module Juvix.Data.Universe where

import Juvix.Data.Fixity
import Juvix.Data.Keyword
import Juvix.Data.Keyword.All (kwType)
import Juvix.Data.Loc
import Juvix.Prelude.Base

data Universe = Universe
  { _universeLevel :: Maybe Natural,
    _universeKw :: KeywordRef,
    _universeLevelLoc :: Maybe Interval
  }
  deriving stock (Show, Ord, Data)

newtype SmallUniverse = SmallUniverse
  { _smallUniverseLoc :: Interval
  }
  deriving stock (Generic, Show, Data)

instance Eq SmallUniverse where
  _ == _ = True

instance Hashable SmallUniverse

getUniverseLevel :: Universe -> Natural
getUniverseLevel Universe {..} = fromMaybe defaultLevel _universeLevel

instance Eq Universe where
  (==) = (==) `on` getUniverseLevel

mkUniverse :: Maybe Natural -> Interval -> Universe
mkUniverse lvl loc =
  Universe
    { _universeLevel = lvl,
      _universeLevelLoc = Nothing,
      _universeKw =
        KeywordRef
          { _keywordRefInterval = loc,
            _keywordRefKeyword = kwType,
            _keywordRefUnicode = Ascii
          }
    }

defaultLevel :: Natural
defaultLevel = 0

smallLevel :: Natural
smallLevel = 0

makeLenses ''Universe
makeLenses ''SmallUniverse

smallUniverse :: Interval -> Universe
smallUniverse = mkUniverse (Just smallLevel)

isSmallUniverse :: Universe -> Bool
isSmallUniverse = (== smallLevel) . getUniverseLevel

smallUniverseNoLoc :: Universe
smallUniverseNoLoc = smallUniverse (error "Universe with no location")

instance HasAtomicity Universe where
  atomicity u = case u ^. universeLevel of
    Nothing -> Atom
    Just {} -> Aggregate appFixity

instance HasAtomicity SmallUniverse where
  atomicity _ = Atom

instance HasLoc Universe where
  getLoc = getLoc . (^. universeKw)

instance HasLoc SmallUniverse where
  getLoc = (^. smallUniverseLoc)
