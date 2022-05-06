module MiniJuvix.Syntax.Concrete.Parser.ParsedItem where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc

data ParsedItem = ParsedItem
  { _parsedLoc :: Interval,
    _parsedTag :: ParsedItemTag
  }
  deriving stock (Show, Eq, Generic)

data ParsedItemTag
  = ParsedTagKeyword
  | ParsedTagLiteralInt
  | ParsedTagLiteralString
  deriving stock (Eq, Show, Generic)

makeLenses ''ParsedItem

instance Hashable ParsedItem

instance HasLoc ParsedItem where
  getLoc = (^. parsedLoc)

instance Hashable ParsedItemTag
