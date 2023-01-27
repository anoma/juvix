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

judocText :: Members '[InfoTableBuilder] r => ParsecS r a -> ParsecS r a
judocText c = do
  (a, i) <- interval c
  P.lift (registerJudocText i)
  return a

judocText_ :: Members '[InfoTableBuilder] r => ParsecS r a -> ParsecS r ()
judocText_ = void . judocText

space :: forall r. Members '[InfoTableBuilder] r => ParsecS r ()
space = space' True >>= mapM_ (P.lift . registerComment)

lexeme :: (Members '[InfoTableBuilder] r) => ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

symbol :: (Members '[InfoTableBuilder] r) => Text -> ParsecS r ()
symbol = void . L.symbol space

lexemeInterval :: (Members '[InfoTableBuilder] r) => ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

decimal :: (Members '[InfoTableBuilder] r, Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

identifier :: (Members '[InfoTableBuilder] r) => ParsecS r Text
identifier = fmap fst identifierL

identifierL :: (Members '[InfoTableBuilder] r) => ParsecS r (Text, Interval)
identifierL = lexeme bareIdentifier

integer :: (Members '[InfoTableBuilder] r) => ParsecS r (Integer, Interval)
integer = integer' decimal

bracedString :: forall e m. (MonadParsec e Text m) => m Text
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

string :: (Members '[InfoTableBuilder] r) => ParsecS r (Text, Interval)
string = lexemeInterval string'

judocExampleStart :: ParsecS r ()
judocExampleStart = P.chunk Str.judocExample >> hspace

judocStart :: ParsecS r ()
judocStart = P.chunk Str.judocStart >> hspace

judocEmptyLine :: (Members '[InfoTableBuilder] r) => ParsecS r ()
judocEmptyLine = lexeme (void (P.try (judocStart >> P.newline)))

kw :: Member InfoTableBuilder r => Keyword -> ParsecS r KeywordRef
kw k = lexeme $ kw' k >>= P.lift . registerKeyword

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: ParsecS r (Text, Interval)
bareIdentifier = interval (rawIdentifier allKeywordStrings)

dot :: forall e m. (MonadParsec e Text m) => m Char
dot = P.char '.'

dottedIdentifier :: (Members '[InfoTableBuilder] r) => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

lbrace :: (Members '[InfoTableBuilder] r) => ParsecS r ()
lbrace = symbol "{"

rbrace :: (Members '[InfoTableBuilder] r) => ParsecS r ()
rbrace = symbol "}"

lparen :: (Members '[InfoTableBuilder] r) => ParsecS r ()
lparen = symbol "("

rparen :: (Members '[InfoTableBuilder] r) => ParsecS r ()
rparen = symbol ")"

parens :: (Members '[InfoTableBuilder] r) => ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: (Members '[InfoTableBuilder] r) => ParsecS r a -> ParsecS r a
braces = between (symbol "{") (symbol "}")
