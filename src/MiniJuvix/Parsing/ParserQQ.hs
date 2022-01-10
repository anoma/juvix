{-# LANGUAGE TemplateHaskell #-}

module MiniJuvix.Parsing.ParserQQ
  ( module MiniJuvix.Parsing.Language,
    module MiniJuvix.Parsing.ParserQQ,
  )
where

import Data.Text (pack, unpack)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Q)
import MiniJuvix.Parsing.Language
import MiniJuvix.Parsing.Parser
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
