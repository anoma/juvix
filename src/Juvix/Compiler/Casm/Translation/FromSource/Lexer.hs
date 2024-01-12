module Juvix.Compiler.Casm.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Compiler.Casm.Translation.FromSource.Lexer,
    module Juvix.Compiler.Casm.Keywords,
  )
where

import Juvix.Compiler.Asm.Translation.FromSource.Lexer hiding (allKeywordStrings, allKeywords, bareIdentifier, identifier)
import Juvix.Compiler.Casm.Keywords
import Juvix.Prelude

offset :: ParsecS r Int16
offset = fromIntegral . fst <$> number (-(2 ^ (15 :: Int16))) (2 ^ (15 :: Int16))

int :: ParsecS r Int
int = fst <$> number (-(2 ^ (31 :: Int))) (2 ^ (31 :: Int))

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings
