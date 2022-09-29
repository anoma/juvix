module Juvix.Data.Keyword where

import Data.HashSet qualified as HashSet
import Juvix.Prelude
import Juvix.Prelude.Pretty

data Keyword = Keyword
  { _keywordAscii :: Text,
    _keywordUnicode :: Maybe Text
  }

makeLenses ''Keyword

instance Pretty Keyword where
  pretty k = maybe (pretty (k ^. keywordAscii)) pretty (k ^. keywordUnicode)

keywordsStrings :: [Keyword] -> HashSet Text
keywordsStrings = HashSet.fromList . concatMap keywordStrings

keywordStrings :: Keyword -> [Text]
keywordStrings Keyword {..} = maybe id (:) _keywordUnicode [_keywordAscii]

asciiKw :: Text -> Keyword
asciiKw _keywordAscii = Keyword {_keywordUnicode = Nothing, ..}

isReservedChar :: Char -> Bool
isReservedChar = (`elem` reservedSymbols)

hasReservedChar :: Text -> Bool
hasReservedChar = any isReservedChar . unpack

reservedSymbols :: [Char]
reservedSymbols = ";(){}.@\"[]"
