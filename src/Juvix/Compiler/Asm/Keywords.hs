module Juvix.Compiler.Asm.Keywords
  ( module Juvix.Compiler.Asm.Keywords,
    module Juvix.Compiler.Tree.Keywords.Base,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Compiler.Tree.Keywords.Base
import Juvix.Data.Keyword.All (kwDollar)
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords = baseKeywords ++ [kwDollar]
