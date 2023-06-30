module Juvix.Compiler.VM.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Compiler.VM.Translation.FromSource.Lexer,
  )
where

import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Prelude
import Juvix.Prelude.Parsing qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

register :: ParsecS r Int
register = P.try $ do
  _ <- P.char 'r'
  lexeme L.decimal

offset :: ParsecS r Int
offset = fst <$> number 0 128

smallInt :: ParsecS r Int
smallInt = fst <$> number (-(2 ^ (24 :: Int))) (2 ^ (24 :: Int))
