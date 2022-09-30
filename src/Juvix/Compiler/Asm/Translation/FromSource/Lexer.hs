module Juvix.Compiler.Asm.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
    module Juvix.Compiler.Asm.Keywords,
  )
where

import Juvix.Compiler.Asm.Keywords
import Juvix.Parser.Lexer
import Juvix.Prelude
import Text.Megaparsec as P hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char.Lexer qualified as L

space :: ParsecS r ()
space = space' False void

lexeme :: ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

lexemeInterval :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

symbol :: Text -> ParsecS r ()
symbol = void . L.symbol space

decimal :: (Member (Reader ParserParams) r, Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

integer :: Member (Reader ParserParams) r => ParsecS r (Integer, Interval)
integer = integer' decimal

number :: Member (Reader ParserParams) r => Int -> Int -> ParsecS r (Int, Interval)
number = number' integer

string :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
string = lexemeInterval string'

kw :: Members '[Reader ParserParams] r => Keyword -> ParsecS r ()
kw k = void $ lexeme $ kw' k

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywordStrings

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
