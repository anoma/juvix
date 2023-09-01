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

    kwAbove,
    kwAlias,
    kwAs,
    kwAssign,
    kwAssoc,
    kwAt,
    kwAxiom,
    kwBelow,
    kwBinary,
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
    kwLeft,
    kwLet,
    kwMapsTo,
    kwModule,
    kwNone,
    kwOpen,
    kwOperator,
    kwPipe,
    kwPositive,
    kwPublic,
    kwRight,
    kwRightArrow,
    kwSame,
    kwSyntax,
    kwTerminating,
    kwType,
    kwUnary,
    kwUsing,
    kwWhere,
    kwWildcard,
  )
import Juvix.Prelude

allKeywordStrings :: HashSet Text
allKeywordStrings = keywordsStrings reservedKeywords

reservedKeywords :: [Keyword]
reservedKeywords =
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
