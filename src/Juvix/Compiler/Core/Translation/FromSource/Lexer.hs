module Juvix.Compiler.Core.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Core.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
    module Juvix.Compiler.Core.Keywords,
  )
where

import Juvix.Compiler.Core.Keywords
import Juvix.Extra.Strings qualified as Str
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

kw :: Keyword -> ParsecS r ()
kw = void . lexeme . kw'

decimal :: (Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

integer :: ParsecS r (Integer, Interval)
integer = integer' decimal

number :: Int -> Int -> ParsecS r (Int, Interval)
number = number' integer

string :: ParsecS r (Text, Interval)
string = lexemeInterval string'

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier allKeywordStrings

symbolAt :: ParsecS r ()
symbolAt = symbol Str.at_

lambda :: ParsecS r ()
lambda = symbol Str.lambdaUnicode <|> symbol Str.lambdaAscii

lbrace :: ParsecS r ()
lbrace = symbol "{"

rbrace :: ParsecS r ()
rbrace = symbol "}"

lparen :: ParsecS r ()
lparen = symbol "("

rparen :: ParsecS r ()
rparen = symbol ")"

parens :: ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: ParsecS r a -> ParsecS r a
braces = between (symbol "{") (symbol "}")
