module Juvix.Compiler.VM.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Compiler.VM.Translation.FromSource.Lexer,
  )
where

import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Prelude
import Juvix.Prelude.Parsing qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

registerR :: ParsecS r Int
registerR = P.try $ do
  _ <- P.char 'r'
  i <- lexeme L.decimal
  return (i + 2)

offset :: ParsecS r Int
offset = fst <$> number 0 128

smallInt :: ParsecS r Int
smallInt = fst <$> number (-(2 ^ (24 :: Int))) (2 ^ (24 :: Int))
