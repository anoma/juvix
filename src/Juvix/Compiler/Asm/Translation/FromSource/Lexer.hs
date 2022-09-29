module Juvix.Compiler.Asm.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Asm.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
  )
where

import Juvix.Extra.Strings qualified as Str
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

keyword :: Text -> ParsecS r ()
keyword = keyword' space

keywordSymbol :: Text -> ParsecS r ()
keywordSymbol = keywordSymbol' space

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier' (`elem` specialSymbols) allKeywords

specialSymbols :: [Char]
specialSymbols = ":"

allKeywords :: [ParsecS r ()]
allKeywords =
  [ kwFun,
    kwInductive,
    kwColon,
    kwSemicolon,
    kwStar,
    kwArrow,
    kwTrue,
    kwFalse,
    kwArg,
    kwTmp
  ]

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

kwFun :: ParsecS r ()
kwFun = keyword Str.fun_

kwInductive :: ParsecS r ()
kwInductive = keyword Str.inductive

kwColon :: ParsecS r ()
kwColon = keyword Str.colon

kwSemicolon :: ParsecS r ()
kwSemicolon = keyword Str.semicolon

kwStar :: ParsecS r ()
kwStar = keyword Str.mul

kwDollar :: ParsecS r ()
kwDollar = keyword Str.dollar

kwArrow :: ParsecS r ()
kwArrow = keyword Str.toAscii <|> keyword Str.toUnicode

kwTrue :: ParsecS r ()
kwTrue = keyword Str.true_

kwFalse :: ParsecS r ()
kwFalse = keyword Str.false_

kwUnit :: ParsecS r ()
kwUnit = keyword Str.unit

kwVoid :: ParsecS r ()
kwVoid = keyword Str.void

kwArg :: ParsecS r ()
kwArg = keyword Str.arg_

kwTmp :: ParsecS r ()
kwTmp = keyword Str.tmp_
