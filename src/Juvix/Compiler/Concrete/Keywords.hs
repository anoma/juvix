module Juvix.Compiler.Concrete.Keywords
  ( module Juvix.Compiler.Concrete.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( -- delimiters
    delimBraceL,
    delimBraceR,
    delimJudocBlockEnd,
    delimJudocBlockStart,
    delimJudocExample,
    delimJudocStart,
    delimParenL,
    delimParenR,
    delimSemicolon,
    -- keywords

    kwAlias,
    kwAs,
    kwAssign,
    kwAt,
    kwAxiom,
    kwBracketL,
    kwBracketR,
    kwBuiltin,
    kwCase,
    kwColon,
    kwEnd,
    kwEq,
    kwFixity,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwIterator,
    kwLambda,
    kwLet,
    kwMapsTo,
    kwModule,
    kwOpen,
    kwOperator,
    kwPipe,
    kwPositive,
    kwPublic,
    kwRightArrow,
    kwSyntax,
    kwTerminating,
    kwType,
    kwUsing,
    kwWhere,
    kwWildcard,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings allKeywords

allKeywords :: [Keyword]
allKeywords =
  [ delimSemicolon,
    kwAssign,
    kwAt,
    kwAxiom,
    kwCase,
    kwColon,
    kwEnd,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwLambda,
    kwLet,
    kwModule,
    kwOpen,
    kwPipe,
    kwPublic,
    kwRightArrow,
    kwSyntax,
    kwType,
    kwUsing,
    kwWhere,
    kwWildcard
  ]

-- | Keywords that do not need to be reserved. Currently only for documentation
-- purposes
nonKeywords :: [Keyword]
nonKeywords =
  [ kwAs,
    kwEq,
    kwFixity,
    kwOperator,
    kwAlias,
    kwIterator
  ]
