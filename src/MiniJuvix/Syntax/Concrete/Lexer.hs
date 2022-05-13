module MiniJuvix.Syntax.Concrete.Lexer where

import Data.Text qualified as Text
import GHC.Unicode
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base hiding (Pos, space)
import MiniJuvix.Syntax.Concrete.Base qualified as P
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder
import Text.Megaparsec.Char.Lexer qualified as L

type OperatorSym = Text

type ParsecS r = ParsecT Void Text (Sem r)

newtype ParserParams = ParserParams
  { _parserParamsRoot :: FilePath
  }

makeLenses ''ParserParams

space :: forall m e. MonadParsec e Text m => m ()
space = L.space space1 lineComment block
  where
    lineComment :: m ()
    lineComment = L.skipLineComment "--"
    block :: m ()
    block = L.skipBlockComment "{-" "-}"

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme space

symbol :: MonadParsec e Text m => Text -> m ()
symbol = void . L.symbol space

decimal :: (MonadParsec e Text m, Num n) => m n
decimal = lexeme L.decimal

identifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Text
identifier = fmap fst identifierL

identifierL :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
identifierL = lexeme bareIdentifier

fromPos :: P.Pos -> Pos
fromPos = Pos . fromIntegral . P.unPos

integer :: MonadParsec e Text m => m Integer
integer = do
  minus <- optional (char '-')
  nat <- lexeme L.decimal
  case minus of
    Nothing -> return nat
    _ -> return (-nat)

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

string :: MonadParsec e Text m => m Text
string = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

mkLoc :: Member (Reader ParserParams) r => Int -> SourcePos -> Sem r Loc
mkLoc offset SourcePos {..} = do
  root <- asks (^. parserParamsRoot)
  let _locFile = normalise (root </> sourceName)
  return Loc {..}
  where
    _locOffset = Pos (fromIntegral offset)
    _locFileLoc = FileLoc {..}
      where
        _locLine = fromPos sourceLine
        _locCol = fromPos sourceColumn

curLoc :: Member (Reader ParserParams) r => ParsecS r Loc
curLoc = do
  sp <- getSourcePos
  offset <- stateOffset <$> getParserState
  lift (mkLoc offset sp)

interval :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

keyword :: Members '[Reader ParserParams, InfoTableBuilder] r => Text -> ParsecS r ()
keyword kw = do
  l <- snd <$> interval (symbol kw)
  lift (registerKeyword l)

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Text, Interval)
bareIdentifier = interval $ do
  notFollowedBy (choice allKeywords)
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP Nothing validChar
  return (Text.cons h t)
  where
    validChar :: Char -> Bool
    validChar c =
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

braces :: MonadParsec e Text m => m a -> m a
braces = between (symbol "{") (symbol "}")

allKeywords :: Members '[Reader ParserParams, InfoTableBuilder] r => [ParsecS r ()]
allKeywords =
  [ kwAssignment,
    kwAxiom,
    kwColon,
    kwColonOmega,
    kwColonOne,
    kwColonZero,
    kwCompile,
    kwEnd,
    kwEval,
    kwForeign,
    kwHiding,
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

lparen :: MonadParsec e Text m => m ()
lparen = symbol "("

rparen :: MonadParsec e Text m => m ()
rparen = symbol ")"

parens :: MonadParsec e Text m => m a -> m a
parens = between lparen rparen

kwAssignment :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAssignment = keyword Str.assignUnicode <|> keyword Str.assignAscii

kwAxiom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwAxiom = keyword Str.axiom

-- | Note that the trailing space is needed to distinguish it from ':='.
kwColon :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwColon = keyword Str.colonSpace

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

kwWildcard :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
kwWildcard = keyword Str.underscore

ghc :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
ghc = keyword Str.ghc

cBackend :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
cBackend = keyword Str.cBackend
