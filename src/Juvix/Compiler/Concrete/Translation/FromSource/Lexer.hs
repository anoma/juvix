module Juvix.Compiler.Concrete.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Concrete.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
  )
where

import Data.Text qualified as Text
import GHC.Unicode
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
import Juvix.Compiler.Concrete.Extra hiding (Pos, space, string')
import Juvix.Compiler.Concrete.Extra qualified as P
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

-- | We use the ascii version in the label so it shows in the error messages.
keywordUnicode :: Members '[Reader ParserParams, InfoTableBuilder] r => Text -> Text -> ParsecS r ()
keywordUnicode ascii unic = P.label (unpack ascii) (keyword ascii <|> keyword unic)

keyword :: Members '[Reader ParserParams, InfoTableBuilder] r => Text -> ParsecS r ()
keyword kw = do
  l <- keywordL' space kw
  lift (registerKeyword l)

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
bareIdentifier = interval $ rawIdentifier allKeywords

dot :: forall e m. MonadParsec e Text m => m Char
dot = P.char '.'

dottedIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

allKeywords :: Members '[Reader ParserParams, InfoTableBuilder] r => [ParsecS r ()]
allKeywords =
  [ kwAssign,
    kwAxiom,
    kwColon,
    kwColonOmega,
    kwColonOne,
    kwColonZero,
    kwCompile,
    kwEnd,
    kwForeign,
    kwHiding,
    kwHole,
    kwImport,
    kwIn,
    kwInductive,
    kwInfix,
    kwInfixl,
    kwInfixr,
    kwLambda,
    kwLet,
    kwModule,
    kwOpen,
    kwPostfix,
    kwPublic,
    kwRightArrow,
    kwSemicolon,
    kwType,
    kwUsing,
    kwWhere,
    kwWildcard
  ]

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

kwBuiltin :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwBuiltin = keyword Str.builtin

kwAssign :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAssign = keyword Str.assignAscii

kwAxiom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAxiom = keyword Str.axiom

kwColon :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColon = keyword Str.colon

kwColonOmega :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonOmega = keywordUnicode Str.colonOmegaAscii Str.colonOmegaUnicode

kwColonOne :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonOne = keyword Str.colonOne

kwColonZero :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonZero = keyword Str.colonZero

kwCompile :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwCompile = keyword Str.compile

kwEnd :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwEnd = keyword Str.end

kwHiding :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwHiding = keyword Str.hiding

kwImport :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwImport = keyword Str.import_

kwForeign :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwForeign = keyword Str.foreign_

kwIn :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwIn = keyword Str.in_

kwInductive :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwInductive = keyword Str.inductive

kwInfix :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwInfix = keyword Str.infix_

kwInfixl :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwInfixl = keyword Str.infixl_

kwInfixr :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwInfixr = keyword Str.infixr_

kwLambda :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwLambda = keywordUnicode Str.lambdaAscii Str.lambdaUnicode

kwLet :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwLet = keyword Str.let_

kwMapsTo :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwMapsTo = keywordUnicode Str.mapstoAscii Str.mapstoUnicode

kwModule :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwModule = keyword Str.module_

kwOpen :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwOpen = keyword Str.open

kwPostfix :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPostfix = keyword Str.postfix

kwPublic :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPublic = keyword Str.public

kwRightArrow :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwRightArrow = keywordUnicode Str.toAscii Str.toUnicode

kwSemicolon :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwSemicolon = keyword Str.semicolon

kwType :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwType = keyword Str.type_

kwTerminating :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwTerminating = keyword Str.terminating

kwPositive :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPositive = keyword Str.positive

kwUsing :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwUsing = keyword Str.using

kwWhere :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwWhere = keyword Str.where_

kwHole :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwHole = keyword Str.underscore

kwWildcard :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwWildcard = keyword Str.underscore

ghc :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
ghc = keyword Str.ghc

cBackend :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
cBackend = keyword Str.cBackend
