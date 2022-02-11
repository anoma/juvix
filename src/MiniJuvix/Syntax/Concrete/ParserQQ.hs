{-# LANGUAGE TemplateHaskell #-}

module MiniJuvix.Syntax.Concrete.ParserQQ
  ( module MiniJuvix.Syntax.Concrete.Language,
    module MiniJuvix.Syntax.Concrete.ParserQQ,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Q)
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Parser
import MiniJuvix.Utils.Prelude

mjuvixMod :: QuasiQuoter
mjuvixMod =
  QuasiQuoter
    { quoteExp = qqExp,
      quotePat = const $ fail "Invalid use: Pattern",
      quoteType = const $ fail "Invalid use: Type",
      quoteDec = const $ fail "Invalid use: Declaration"
    }
  where
    qqExp :: String -> Q Exp
    qqExp str = do
      r <- parseOrThrow str
      [|r|]
    parseOrThrow :: String -> Q (Module 'Parsed 'ModuleTop)
    parseOrThrow str =
      case runModuleParser "Quasi Quote" (pack str) of
        Left err -> fail ("Parser Error: " ++ unpack err)
        Right m -> return m
