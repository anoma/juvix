module Juvix.Compiler.Reg.Keywords
  ( module Juvix.Compiler.Reg.Keywords,
    module Juvix.Compiler.Tree.Keywords.Base,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Tree.Keywords.Base
import Juvix.Data.Keyword.All (kwAdd_, kwAlloc, kwAtoi, kwBr, kwCAlloc, kwCCall, kwCCallTail, kwCExtend, kwCall, kwCallTail, kwCase, kwDefault, kwDiv_, kwDollar, kwDump, kwEq, kwEq_, kwLe_, kwLive, kwLt_, kwMod_, kwMul_, kwNop, kwPrealloc, kwRet, kwShow, kwStrcat, kwSub_, kwTrace)
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
         kwEq,
         kwShow,
         kwAtoi,
         kwTrace,
         kwDump,
         kwPrealloc,
         kwAlloc,
         kwCAlloc,
         kwCExtend,
         kwCall,
         kwCallTail,
         kwLive,
         kwCCall,
         kwCCallTail,
         kwRet,
         kwBr,
         kwCase
       ]
