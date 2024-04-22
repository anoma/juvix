module Juvix.Prelude.FlatParse.Lexer
  ( module Juvix.Prelude.FlatParse.Lexer,
    module Juvix.Parser.Lexer,
  )
where

import Juvix.Parser.Lexer (isWhiteSpace, validFirstChar, validTailChar)
import Juvix.Parser.Lexer qualified as L
import Juvix.Prelude.FlatParse

whiteSpace1 :: Parser e ()
whiteSpace1 = skipSome whiteSpaceChar

-- TODO is Ascii version much faster?
whiteSpaceChar :: Parser e ()
-- whiteSpaceChar = skipSatisfyAscii L.isWhiteSpace
whiteSpaceChar = skipSatisfy L.isWhiteSpace
