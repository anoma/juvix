module Juvix.Compiler.Concrete.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Concrete.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
    module Juvix.Data.Keyword,
    module Juvix.Compiler.Concrete.Keywords,
  )
where

import Data.Text qualified as Text
import GHC.Unicode
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
import Juvix.Compiler.Concrete.Extra hiding (Pos, space, string')
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Keywords
import Juvix.Data.Keyword
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Lexer
import Juvix.Prelude
import Text.Megaparsec.Char.Lexer qualified as L

type OperatorSym = Text

comment :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r a
comment c = do
  (a, i) <- interval c
  P.lift (registerComment i)
  return a

comment_ :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r ()
comment_ = void . comment

space :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
space = space' True comment_

lexeme :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

symbol :: Members '[Reader ParserParams, InfoTableBuilder] r => Text -> ParsecS r ()
symbol = void . L.symbol space

lexemeInterval :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

decimal :: (Members '[Reader ParserParams, InfoTableBuilder] r, Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

identifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Text
identifier = fmap fst identifierL

identifierL :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
identifierL = lexeme bareIdentifier

integer :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Integer, Interval)
integer = integer' decimal

bracedString :: forall e m. MonadParsec e Text m => m Text
bracedString =
  Text.strip . unIndent . pack <$> (char '{' >> manyTill (P.try escaped <|> anySingle) (char '}'))
  where
    unIndent :: Text -> Text
    unIndent t = Text.unlines (Text.drop (fromMaybe 0 (indentIdx t)) <$> Text.lines t)
    indentIdx :: Text -> Maybe Int
    indentIdx = minimumMay . mapMaybe firstNonBlankChar . Text.lines
    firstNonBlankChar :: Text -> Maybe Int
    firstNonBlankChar = Text.findIndex (not . isSpace)
    escaped :: m Char
    escaped = do
      void (char '\\')
      char '}'

string :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
string = lexemeInterval string'

judocExampleStart :: ParsecS r ()
judocExampleStart = P.chunk Str.judocExample >> hspace

judocStart :: ParsecS r ()
judocStart = P.chunk Str.judocStart >> hspace

judocEmptyLine :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
judocEmptyLine = lexeme (void (P.try (judocStart >> P.newline)))

kw :: Members '[Reader ParserParams, InfoTableBuilder] r => Keyword -> ParsecS r ()
kw k = lexeme $ kw' k >>= P.lift . registerKeyword

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
bareIdentifier = interval (rawIdentifier allKeywordStrings)

dot :: forall e m. MonadParsec e Text m => m Char
dot = P.char '.'

dottedIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

lbrace :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
lbrace = symbol "{"

rbrace :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
rbrace = symbol "}"

lparen :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
lparen = symbol "("

rparen :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
rparen = symbol ")"

parens :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r a -> ParsecS r a
braces = between (symbol "{") (symbol "}")
