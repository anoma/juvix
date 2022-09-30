module Juvix.Compiler.Asm.Keywords
  ( module Juvix.Compiler.Asm.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( kwArg,
    kwColon,
    kwDollar,
    kwFalse,
    kwFun,
    kwInductive,
    kwRightArrow,
    kwSemicolon,
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
  [ kwFun,
    kwInductive,
    kwColon,
    kwSemicolon,
    kwStar,
    kwRightArrow,
    kwTrue,
    kwFalse,
    kwArg,
    kwTmp
  ]
