module Juvix.Data.Keyword where

import Data.HashSet qualified as HashSet
import Juvix.Prelude
import Juvix.Prelude.Pretty

data Keyword = Keyword
  { _keywordAscii :: Text,
    _keywordUnicode :: Maybe Text,
    -- | true if _keywordAscii has a reserved character (the unicode is assumed to not have any)
    _keywordHasReserved :: Bool
  }

makeLenses ''Keyword

-- | Unicode has preference
instance Pretty Keyword where
  pretty k = maybe (pretty (k ^. keywordAscii)) pretty (k ^. keywordUnicode)

keywordsStrings :: [Keyword] -> HashSet Text
keywordsStrings = HashSet.fromList . concatMap keywordStrings

keywordStrings :: Keyword -> [Text]
keywordStrings Keyword {..} = maybe id (:) _keywordUnicode [_keywordAscii]

mkKw :: Text -> Maybe Text -> Keyword
mkKw _keywordAscii _keywordUnicode = Keyword {
  _keywordHasReserved = hasReservedChar _keywordAscii,
  ..
  }

asciiKw :: Text -> Keyword
asciiKw ascii = mkKw ascii Nothing

unicodeKw :: Text -> Text -> Keyword
unicodeKw ascii unicode = mkKw ascii (Just unicode)

isReservedChar :: Char -> Bool
isReservedChar = (`elem` reservedSymbols)

hasReservedChar :: Text -> Bool
hasReservedChar = any isReservedChar . unpack

reservedSymbols :: [Char]
reservedSymbols = ";(){}.@\"[]"
