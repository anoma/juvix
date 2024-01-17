module Juvix.Compiler.Tree.Keywords.Base
  ( module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
    baseKeywords,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( delimSemicolon,
    kwArg,
    kwColon,
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

baseKeywords :: [Keyword]
baseKeywords =
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
