module Juvix.Compiler.Core.Keywords
  ( module Juvix.Compiler.Core.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( kwAssign,
    kwBind,
    kwCase,
    kwComma,
    kwConstr,
    kwDef,
    kwDiv,
    kwElse,
    kwEq,
    kwFail,
    kwGe,
    kwGt,
    kwIf,
    kwIn,
    kwLe,
    kwLet,
    kwLetRec,
    kwLt,
    kwMatch,
    kwMinus,
    kwMod,
    kwMul,
    kwOf,
    kwPlus,
    kwRightArrow,
    kwSemicolon,
    kwSeq,
    kwThen,
    kwTrace,
    kwWildcard,
    kwWith,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  [ kwAssign,
    kwLet,
    kwLetRec,
    kwIn,
    kwConstr,
    kwCase,
    kwOf,
    kwMatch,
    kwWith,
    kwIf,
    kwThen,
    kwElse,
    kwDef,
    kwRightArrow,
    kwSemicolon,
    kwComma,
    kwWildcard,
    kwPlus,
    kwMinus,
    kwMul,
    kwDiv,
    kwMod,
    kwEq,
    kwLt,
    kwLe,
    kwGt,
    kwGe,
    kwBind,
    kwSeq,
    kwTrace,
    kwFail
  ]
