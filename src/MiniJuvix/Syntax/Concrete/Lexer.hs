module MiniJuvix.Syntax.Concrete.Lexer where

import Data.Text qualified as Text
import GHC.Unicode
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base hiding (Pos, space)
import MiniJuvix.Syntax.Concrete.Base qualified as P
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder
import Text.Megaparsec.Char.Lexer qualified as L

type OperatorSym = Text

type ParsecS r = ParsecT Void Text (Sem r)

newtype ParserParams = ParserParams
  { _parserParamsRoot :: FilePath
  }

makeLenses ''ParserParams

space :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
space = L.space space1 lineComment block
  where
    lineComment :: ParsecS r ()
    lineComment =
      interval (L.skipLineComment "--") >>= lift . registerComment . snd

    block :: ParsecS r ()
    block =
      interval (L.skipBlockComment "{-" "-}") >>= lift . registerComment . snd

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
integer = do
  minus <- optional (char '-')
  (nat, i) <- decimal
  let nat' = case minus of
        Nothing -> nat
        _ -> (-nat)
  return (nat', i)

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
string =
  lexemeInterval $
    pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

curLoc :: Member (Reader ParserParams) r => ParsecS r Loc
curLoc = do
  sp <- getSourcePos
  offset <- getOffset
  root <- lift (asks (^. parserParamsRoot))
  return (mkLoc root offset sp)

interval :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

withLoc :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (WithLoc a)
withLoc ma = do
  (a, i) <- interval ma
  return (WithLoc i a)

keyword :: Members '[Reader ParserParams, InfoTableBuilder] r => Text -> ParsecS r ()
keyword kw = do
  l <- P.try $ do
    i <- snd <$> interval (P.chunk kw)
    notFollowedBy (satisfy validTailChar)
    space
    return i
  lift (registerKeyword l)

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
bareIdentifier = interval $ do
  notFollowedBy (choice allKeywords)
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP Nothing validTailChar
  return (Text.cons h t)

validTailChar :: Char -> Bool
validTailChar c =
  isAlphaNum c || validFirstChar c

validFirstChar :: Char -> Bool
validFirstChar c =
  or
    [ isLetter c,
      cat == MathSymbol,
      cat == CurrencySymbol,
      cat == ModifierLetter,
      c `elem` extraAllowedChars
    ]
  where
    extraAllowedChars :: [Char]
    extraAllowedChars = "_'-*,&"
    cat = generalCategory c

dot :: forall e m. MonadParsec e Text m => m Char
dot = P.char '.'

dottedIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

allKeywords :: Members '[Reader ParserParams, InfoTableBuilder] r => [ParsecS r ()]
allKeywords =
  [ kwAssignment,
    kwAxiom,
    -- kwBuiltin, -- no need to be a reserved keyword
    kwColon,
    kwColonOmega,
    kwColonOne,
    kwColonZero,
    kwCompile,
    kwEnd,
    kwEval,
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
    kwMapsTo,
    kwMatch,
    kwModule,
    kwOpen,
    kwPostfix,
    kwPrint,
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

kwAssignment :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAssignment = keyword Str.assignUnicode <|> keyword Str.assignAscii

kwAxiom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAxiom = keyword Str.axiom

kwColon :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColon = keyword Str.colon

kwColonOmega :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonOmega = keyword Str.colonOmegaUnicode <|> keyword Str.colonOmegaAscii

kwColonOne :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonOne = keyword Str.colonOne

kwColonZero :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColonZero = keyword Str.colonZero

kwCompile :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwCompile = keyword Str.compile

kwEnd :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwEnd = keyword Str.end

kwEval :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwEval = keyword Str.eval

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
kwLambda = keyword Str.lambdaUnicode <|> keyword Str.lambdaAscii

kwLet :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwLet = keyword Str.let_

kwMapsTo :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwMapsTo = keyword Str.mapstoUnicode <|> keyword Str.mapstoAscii

kwMatch :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwMatch = keyword Str.match

kwModule :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwModule = keyword Str.module_

kwOpen :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwOpen = keyword Str.open

kwPostfix :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPostfix = keyword Str.postfix

kwPrint :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPrint = keyword Str.print

kwPublic :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwPublic = keyword Str.public

kwRightArrow :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwRightArrow = keyword Str.toUnicode <|> keyword Str.toAscii

kwSemicolon :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwSemicolon = keyword Str.semicolon

kwType :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwType = keyword Str.type_

kwTerminating :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwTerminating = keyword Str.terminating

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
