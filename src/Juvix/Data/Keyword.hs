module Juvix.Data.Keyword where

import Data.HashSet qualified as HashSet
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data IsUnicode
  = Unicode
  | Ascii
  deriving stock (Eq, Show, Ord, Data)

data Keyword = Keyword
  { _keywordAscii :: Text,
    _keywordUnicode :: Maybe Text,
    -- | true if _keywordAscii has a reserved character (the unicode is assumed to not have any)
    _keywordHasReserved :: Bool
  }
  deriving stock (Eq, Show, Ord, Data)

data KeywordRef = KeywordRef
  { _keywordRefKeyword :: Keyword,
    _keywordRefInterval :: Interval,
    _keywordRefUnicode :: IsUnicode
  }
  deriving stock (Show, Data)

makeLenses ''Keyword
makeLenses ''KeywordRef

instance Eq KeywordRef where
  a == b = a ^. keywordRefKeyword == b ^. keywordRefKeyword

instance Ord KeywordRef where
  compare a b = compare (a ^. keywordRefKeyword, a ^. keywordRefUnicode) (b ^. keywordRefKeyword, b ^. keywordRefUnicode)

instance HasLoc KeywordRef where
  getLoc = (^. keywordRefInterval)

instance Pretty KeywordRef where
  pretty r
    | Unicode <- r ^. keywordRefUnicode = pretty (fromJust (k ^. keywordUnicode))
    | otherwise = pretty (k ^. keywordAscii)
    where
      k :: Keyword
      k = r ^. keywordRefKeyword

-- | Unicode has preference
instance Pretty Keyword where
  pretty k = maybe (pretty (k ^. keywordAscii)) pretty (k ^. keywordUnicode)

keywordsStrings :: [Keyword] -> HashSet Text
keywordsStrings = HashSet.fromList . concatMap keywordStrings

-- | Nothing if it does not match.
keywordMatch :: Keyword -> Text -> Maybe IsUnicode
keywordMatch Keyword {..} t
  | Just t == _keywordUnicode = Just Unicode
  | t == _keywordAscii = Just Ascii
  | otherwise = Nothing

keywordStrings :: Keyword -> [Text]
keywordStrings Keyword {..} = maybe id (:) _keywordUnicode [_keywordAscii]

mkKw :: Text -> Maybe Text -> Keyword
mkKw _keywordAscii _keywordUnicode =
  Keyword
    { _keywordHasReserved = hasReservedChar _keywordAscii,
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
