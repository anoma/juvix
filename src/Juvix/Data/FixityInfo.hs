module Juvix.Data.FixityInfo
  ( module Juvix.Data.FixityInfo,
    module Juvix.Data.Fixity,
  )
where

import Juvix.Data.Fixity (BinaryAssoc (..))
import Juvix.Data.Yaml
import Juvix.Prelude.Base

data Arity
  = Unary
  | Binary
  deriving stock (Show, Eq, Ord, Generic)

-- TODO consider using sum type for Same | Below && Above
data FixityInfo = FixityInfo
  { _fixityArity :: Arity,
    _fixityAssoc :: Maybe BinaryAssoc,
    _fixityPrecSame :: Maybe Text,
    _fixityPrecBelow :: [Text],
    _fixityPrecAbove :: [Text]
  }
  deriving stock (Show, Eq, Ord, Generic)

makeLenses ''FixityInfo

instance FromJSON FixityInfo where
  parseJSON = toAesonParser id parseFixityInfo
    where
      parseFixityInfo :: Parse YamlError FixityInfo
      parseFixityInfo = do
        checkYamlKeys ["arity", "assoc", "same", "below", "above"]
        _fixityArity <- key "arity" parseArity
        _fixityAssoc <- keyMay "assoc" parseAssoc
        _fixityPrecSame <- keyMay "same" asText
        _fixityPrecBelow <- fromMaybe [] <$> keyMay "below" (eachInArray asText)
        _fixityPrecAbove <- fromMaybe [] <$> keyMay "above" (eachInArray asText)
        when (isJust _fixityPrecSame && not (null _fixityPrecBelow && null _fixityPrecAbove)) $
          throwCustomError "'same' cannot be provided together with 'above' or 'below'"
        return FixityInfo {..}

      parseArity :: Parse YamlError Arity
      parseArity = do
        txt <- asText
        case txt of
          "unary" -> return Unary
          "binary" -> return Binary
          _ -> throwCustomError "unknown arity"

      parseAssoc :: Parse YamlError BinaryAssoc
      parseAssoc = do
        txt <- asText
        case txt of
          "left" -> return AssocLeft
          "right" -> return AssocRight
          "none" -> return AssocNone
          _ -> throwCustomError "unknown associativity"
