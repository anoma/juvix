module MiniJuvix.Syntax.Concrete.Parser.ParsedItem where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc

data ParsedItem = ParsedItem
  { _parsedLoc :: Interval,
    _parsedTag :: ParsedItemTag
  }
  deriving stock (Show, Eq, Generic)

instance Hashable ParsedItem

instance HasLoc ParsedItem where
  getLoc = _parsedLoc

data ParsedItemTag
  = ParsedTagKeyword
  | ParsedTagLiteralInt
  | ParsedTagLiteralString
  deriving stock (Eq, Show, Generic)

instance Hashable ParsedItemTag

makeLenses ''ParsedItem
