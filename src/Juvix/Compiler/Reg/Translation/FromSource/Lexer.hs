module Juvix.Compiler.Reg.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Reg.Translation.FromSource.Lexer,
    module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base,
    module Juvix.Compiler.Reg.Keywords,
  )
where

import Juvix.Compiler.Reg.Keywords
import Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
import Juvix.Prelude

int :: ParsecS r Int
int = fst <$> number (-(2 ^ (31 :: Int))) (2 ^ (31 :: Int))

smallnat :: ParsecS r Int
smallnat = fst <$> number 0 256

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings
