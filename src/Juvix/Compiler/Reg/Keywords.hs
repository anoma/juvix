module Juvix.Compiler.Reg.Keywords
  ( module Juvix.Compiler.Reg.Keywords,
    module Juvix.Compiler.Tree.Keywords.Base,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Tree.Keywords.Base
import Juvix.Data.Keyword.All (kwAdd_, kwDiv_, kwDollar, kwEq, kwEq_, kwLe_, kwLt_, kwMod_, kwMul_, kwNop, kwStrcat, kwSub_)
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  baseKeywords
    ++ [ kwNop,
         kwAdd_,
         kwSub_,
         kwMul_,
         kwDiv_,
         kwMod_,
         kwLt_,
         kwLe_,
         kwEq_,
         kwStrcat,
         kwEq
       ]
