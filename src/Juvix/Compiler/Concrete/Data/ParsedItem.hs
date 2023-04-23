module Juvix.Compiler.Concrete.Data.ParsedItem where

import Juvix.Prelude

data ParsedItem = ParsedItem
  { _parsedLoc :: Interval,
    _parsedTag :: ParsedItemTag
  }
  deriving stock (Show, Eq, Generic)

data ParsedItemTag
  = ParsedTagKeyword
  | ParsedTagLiteralInt
  | ParsedTagLiteralString
  | ParsedTagComment
  | ParsedTagJudoc
  deriving stock (Eq, Show, Generic)

makeLenses ''ParsedItem

instance Hashable ParsedItem

instance HasLoc ParsedItem where
  getLoc = (^. parsedLoc)

instance Hashable ParsedItemTag
