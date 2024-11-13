module Juvix.Data.ParsedItem where

import Juvix.Data.Loc
import Juvix.Prelude.Base

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
  | ParsedTagPragma
  | ParsedTagJudoc
  | ParsedTagDelimiter
  deriving stock (Eq, Show, Generic)

makeLenses ''ParsedItem

instance Hashable ParsedItem

instance HasLoc ParsedItem where
  getLoc = (^. parsedLoc)

instance Hashable ParsedItemTag
