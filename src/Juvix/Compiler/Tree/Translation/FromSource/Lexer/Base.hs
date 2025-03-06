{-# LANGUAGE AllowAmbiguousTypes #-}

module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base
  ( module Juvix.Compiler.Tree.Translation.FromSource.Lexer.Base,
    module Juvix.Parser.Lexer,
  )
where

import Juvix.Data.Keyword
import Juvix.Parser.Error.Base
import Juvix.Parser.Lexer
import Juvix.Prelude
import Text.Megaparsec as P hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char.Lexer qualified as L

space :: ParsecS r ()
space = void (space' False)

lexeme :: ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

lexemeInterval :: ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

symbol :: Text -> ParsecS r ()
symbol = void . L.symbol space

integer :: ParsecS r (WithLoc Integer)
integer = lexeme integer'

number :: forall e r. (FromSimpleParserError e, Member (Error e) r) => Int -> Int -> ParsecS r (WithLoc Int)
number = number' @e integer

field :: ParsecS r (Integer, Interval)
field = lexemeInterval field'

uint8 :: ParsecS r (Integer, Interval)
uint8 = lexemeInterval uint8'

string :: ParsecS r (Text, Interval)
string = lexemeInterval string'

kw :: Keyword -> ParsecS r ()
kw k = void $ lexeme $ kw' k

specialSymbols :: [Char]
specialSymbols = ":"

dot :: ParsecS r ()
dot = symbol "."

comma :: ParsecS r ()
comma = symbol ","

lbrace :: ParsecS r ()
lbrace = symbol "{"

rbrace :: ParsecS r ()
rbrace = symbol "}"

lparen :: ParsecS r ()
lparen = symbol "("

rparen :: ParsecS r ()
rparen = symbol ")"

lbracket :: ParsecS r ()
lbracket = symbol "["

rbracket :: ParsecS r ()
rbracket = symbol "]"

parens :: ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: ParsecS r a -> ParsecS r a
braces = between (symbol "{") (symbol "}")

brackets :: ParsecS r a -> ParsecS r a
brackets = between (symbol "[") (symbol "]")
