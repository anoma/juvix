module Juvix.Compiler.Tree.Keywords
  ( module Juvix.Compiler.Tree.Keywords,
    module Juvix.Compiler.Tree.Keywords.Base,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Tree.Keywords.Base
import Juvix.Data.Keyword.All
  ( kwAdd_,
    kwAlloc,
    kwArgsNum,
    kwAtoi,
    kwBr,
    kwCAlloc,
    kwCCall,
    kwCExtend,
    kwCall,
    kwCase,
    kwDiv_,
    kwEq_,
    kwFail,
    kwLe_,
    kwLt_,
    kwMod_,
    kwMul_,
    kwSave,
    kwSeq_,
    kwShow,
    kwStrConcat,
    kwSub_,
    kwTrace,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  baseKeywords
    ++ [ kwAdd_,
         kwSub_,
         kwMul_,
         kwDiv_,
         kwLt_,
         kwLe_,
         kwSeq_,
         kwEq_,
         kwStrConcat,
         kwShow,
         kwAtoi,
         kwTrace,
         kwFail,
         kwArgsNum,
         kwAlloc,
         kwCAlloc,
         kwCExtend,
         kwCall,
         kwCCall,
         kwBr,
         kwCase,
         kwSave
       ]
