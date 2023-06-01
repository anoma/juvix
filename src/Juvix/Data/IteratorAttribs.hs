module Juvix.Data.IteratorAttribs where

import Juvix.Data.Yaml
import Juvix.Prelude.Base

data IteratorAttribs = IteratorAttribs
  { _iteratorAttribsInitNum :: Maybe Int,
    _iteratorAttribsRangeNum :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON IteratorAttribs where
  parseJSON = toAesonParser id parseAttribs
    where
      parseAttribs :: Parse YamlError IteratorAttribs
      parseAttribs = do
        checkYamlKeys ["init", "range"]
        _iteratorAttribsInitNum <- keyMay "init" asIntegral
        _iteratorAttribsRangeNum <- keyMay "range" asIntegral
        return IteratorAttribs {..}

emptyIteratorAttribs :: IteratorAttribs
emptyIteratorAttribs =
  IteratorAttribs
    { _iteratorAttribsInitNum = Nothing,
      _iteratorAttribsRangeNum = Nothing
    }
