module Juvix.Compiler.Casm.Keywords
  ( module Juvix.Compiler.Casm.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( delimSemicolon,
    kwAbs,
    kwAp,
    kwApPlusPlus,
    kwCall,
    kwColon,
    kwDiv,
    kwEq,
    kwFp,
    kwIf,
    kwIntAdd,
    kwIntDiv,
    kwIntLt,
    kwIntMod,
    kwIntMul,
    kwIntSub,
    kwJmp,
    kwMinus,
    kwMul,
    kwNop,
    kwNotEq,
    kwPlus,
    kwPlusEq,
    kwRel,
    kwRet,
    kwTrace,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  [ delimSemicolon,
    kwAbs,
    kwAp,
    kwApPlusPlus,
    kwCall,
    kwColon,
    kwDiv,
    kwEq,
    kwFp,
    kwIf,
    kwIntAdd,
    kwIntDiv,
    kwIntLt,
    kwIntMod,
    kwIntMul,
    kwIntSub,
    kwJmp,
    kwMinus,
    kwMul,
    kwNop,
    kwNotEq,
    kwPlus,
    kwPlusEq,
    kwRel,
    kwRet,
    kwTrace
  ]
