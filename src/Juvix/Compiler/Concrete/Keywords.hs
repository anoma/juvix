module Juvix.Compiler.Concrete.Keywords
  ( module Juvix.Compiler.Concrete.Keywords,
    module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Data.Keyword.All
  ( -- reserved

    -- extra
    cBackend,
    ghc,
    kwAssign,
    kwAt,
    kwAxiom,
    kwBuiltin,
    kwCase,
    kwColon,
    kwColonOmega,
    kwColonOne,
    kwColonZero,
    kwCompile,
    kwEnd,
    kwForeign,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwInfix,
    kwInfixl,
    kwInfixr,
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
    kwColonOmega,
    kwColonOne,
    kwColonZero,
    kwCompile,
    kwEnd,
    kwForeign,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwInfix,
    kwInfixl,
    kwInfixr,
    kwLambda,
    kwLet,
    kwModule,
    kwOpen,
    kwPipe,
    kwPostfix,
    kwPublic,
    kwRightArrow,
    kwSemicolon,
    kwType,
    kwUsing,
    kwWhere,
    kwWildcard
  ]
