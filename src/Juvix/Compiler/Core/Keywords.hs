module Juvix.Compiler.Core.Keywords
  ( module Juvix.Compiler.Core.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( kwAny,
    kwAssign,
    kwBind,
    kwCase,
    kwColon,
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
    kwInductive,
    kwLe,
    kwLet,
    kwLetRec,
    kwLt,
    kwMatch,
    kwMinus,
    kwMod,
    kwMul,
    kwOf,
    kwPi,
    kwPlus,
    kwRightArrow,
    kwSemicolon,
    kwSeq,
    kwThen,
    kwTrace,
    kwType,
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
    kwInductive,
    kwCase,
    kwOf,
    kwMatch,
    kwWith,
    kwIf,
    kwThen,
    kwElse,
    kwDef,
    kwRightArrow,
    kwColon,
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
    kwFail,
    kwAny,
    kwPi,
    kwType
  ]
