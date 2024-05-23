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
    kwAnomaDecode,
    kwAnomaEncode,
    kwAnomaGet,
    kwAnomaSign,
    kwAnomaVerifyDetached,
    kwArgsNum,
    kwAtoi,
    kwBr,
    kwCAlloc,
    kwCCall,
    kwCExtend,
    kwCall,
    kwCase,
    kwDiv_,
    kwEcOp,
    kwEq_,
    kwFail,
    kwFieldAdd,
    kwFieldDiv,
    kwFieldMul,
    kwFieldSub,
    kwLe_,
    kwLt_,
    kwMod_,
    kwMul_,
    kwPoseidon,
    kwRandomEcPoint,
    kwSave,
    kwSeq_,
    kwShow,
    kwStrcat,
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
         kwFieldAdd,
         kwFieldSub,
         kwFieldMul,
         kwFieldDiv,
         kwSeq_,
         kwEq_,
         kwStrcat,
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
         kwSave,
         kwAnomaGet,
         kwAnomaDecode,
         kwAnomaEncode,
         kwAnomaVerifyDetached,
         kwAnomaSign,
         kwPoseidon,
         kwEcOp,
         kwRandomEcPoint
       ]
