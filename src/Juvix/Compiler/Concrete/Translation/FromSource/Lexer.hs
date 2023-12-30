module Juvix.Compiler.Concrete.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Concrete.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
    module Juvix.Data.Keyword,
    module Juvix.Compiler.Concrete.Keywords,
  )
where

import Data.Text qualified as Text
import GHC.Unicode
import Juvix.Compiler.Concrete.Extra hiding (Pos, hspace, space, string')
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder
import Juvix.Data.Keyword
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Lexer
import Juvix.Prelude
import Text.Megaparsec.Char.Lexer qualified as L

type OperatorSym = Text

judocText :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r a
judocText c = do
  (a, i) <- interval c
  P.lift (registerJudocText i)
  return a

judocText_ :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r ()
judocText_ = void . judocText

space :: forall r. (Members '[ParserResultBuilder] r) => ParsecS r ()
space = space' True >>= mapM_ (P.lift . registerSpaceSpan)

lexeme :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

symbol :: (Members '[ParserResultBuilder] r) => Text -> ParsecS r ()
symbol = void . L.symbol space

lexemeInterval :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

decimal :: (Members '[ParserResultBuilder] r, Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

identifier :: (Members '[ParserResultBuilder] r) => ParsecS r Text
identifier = fmap fst identifierL

identifierL :: (Members '[ParserResultBuilder] r) => ParsecS r (Text, Interval)
identifierL = lexeme bareIdentifier

integer :: (Members '[ParserResultBuilder] r) => ParsecS r (WithLoc Integer)
integer = do
  (num, i) <- integer' decimal
  return (WithLoc i num)

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

string :: (Members '[ParserResultBuilder] r) => ParsecS r (Text, Interval)
string = lexemeInterval string'

judocExampleStart :: ParsecS r ()
judocExampleStart = P.chunk Str.judocExample >> hspace_

judocBlockEnd :: (Members '[ParserResultBuilder] r) => ParsecS r KeywordRef
judocBlockEnd = kw delimJudocBlockEnd

judocBlockStart :: (Members '[ParserResultBuilder] r) => ParsecS r KeywordRef
judocBlockStart = kwBare delimJudocBlockStart

judocStart :: (Members '[ParserResultBuilder] r) => ParsecS r KeywordRef
judocStart = kwBare delimJudocStart <* hspace_

-- | Does not consume space after it
kwBare :: (Member ParserResultBuilder r) => Keyword -> ParsecS r KeywordRef
kwBare k = kw' k >>= P.lift . registerKeyword

kw :: (Member ParserResultBuilder r) => Keyword -> ParsecS r KeywordRef
kw = lexeme . kwBare

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: ParsecS r (Text, Interval)
bareIdentifier = interval (rawIdentifier allKeywordStrings)

dot :: forall e m. (MonadParsec e Text m) => m Char
dot = P.char '.'

dottedIdentifier :: (Members '[ParserResultBuilder] r) => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

delim :: (Members '[ParserResultBuilder] r) => Text -> ParsecS r ()
delim sym = lexeme $ delim' sym >>= P.lift . registerDelimiter

lbrace :: (Members '[ParserResultBuilder] r) => ParsecS r ()
lbrace = delim "{"

rbrace :: (Members '[ParserResultBuilder] r) => ParsecS r ()
rbrace = delim "}"

ldoubleBrace :: (Members '[ParserResultBuilder] r) => ParsecS r ()
ldoubleBrace = delim "{{"

rdoubleBrace :: (Members '[ParserResultBuilder] r) => ParsecS r ()
rdoubleBrace = delim "}}"

lparen :: (Members '[ParserResultBuilder] r) => ParsecS r ()
lparen = delim "("

rparen :: (Members '[ParserResultBuilder] r) => ParsecS r ()
rparen = delim ")"

pipe :: (Members '[ParserResultBuilder] r) => ParsecS r ()
pipe = delim "|"

semicolon :: (Members '[ParserResultBuilder] r) => ParsecS r ()
semicolon = delim ";"

parens :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r a
braces = between lbrace rbrace

doubleBraces :: (Members '[ParserResultBuilder] r) => ParsecS r a -> ParsecS r a
doubleBraces = between ldoubleBrace rdoubleBrace
