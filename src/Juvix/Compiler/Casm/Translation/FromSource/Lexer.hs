module Juvix.Compiler.Casm.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Compiler.Casm.Translation.FromSource.Lexer,
  )
where

import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Prelude

offset :: ParsecS r Int16
offset = fromIntegral . fst <$> number (-(2 ^ (15 :: Int16))) (2 ^ (15 :: Int16))

int :: ParsecS r Int
int = fst <$> number (-(2 ^ (31 :: Int))) (2 ^ (31 :: Int))
