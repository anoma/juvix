module Juvix.Compiler.Asm.Keywords
  ( module Juvix.Compiler.Asm.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( delimSemicolon,
    kwArg,
    kwColon,
    kwDollar,
    kwFalse,
    kwFun,
    kwInductive,
    kwRightArrow,
    kwStar,
    kwTmp,
    kwTrue,
    kwUnit,
    kwVoid,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  [ delimSemicolon,
    kwFun,
    kwInductive,
    kwColon,
    kwStar,
    kwRightArrow,
    kwTrue,
    kwFalse,
    kwArg,
    kwTmp
  ]
