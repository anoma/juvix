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
    -- keywords
    kwAs,
    kwAssign,
    kwAt,
    kwAxiom,
    kwBuiltin,
    kwCase,
    kwColon,
    kwEnd,
    kwEq,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwInfix,
    kwInfixl,
    kwInfixr,
    kwIterator,
    kwLambda,
    kwLet,
    kwMapsTo,
    kwModule,
    kwOpen,
    kwPipe,
    kwPositive,
    kwPostfix,
    kwPublic,
    kwRightArrow,
    kwSemicolon,
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
  [ kwAssign,
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
    kwSemicolon,
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
    kwInfix,
    kwInfixl,
    kwInfixr,
    kwPostfix,
    kwIterator
  ]
