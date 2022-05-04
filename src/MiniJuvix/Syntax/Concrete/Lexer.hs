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

space :: forall m e. MonadParsec e Text m => m ()
space = L.space space1 lineComment block
  where
    lineComment = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme space

symbol :: MonadParsec e Text m => Text -> m ()
symbol = void . L.symbol space

decimal :: (MonadParsec e Text m, Num n) => m n
decimal = lexeme L.decimal

identifier :: Member InfoTableBuilder r => ParsecS r Text
identifier = fmap fst identifierL

identifierL :: Member InfoTableBuilder r => ParsecS r (Text, Interval)
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

-- | TODO allow escaping { inside the string using \{
bracedString :: MonadParsec e Text m => m Text
bracedString =
  Text.strip . unIndent . pack <$> (char '{' >> manyTill anySingle (char '}'))
  where
    unIndent :: Text -> Text
    unIndent t = Text.unlines (Text.drop (fromMaybe 0 (indentIdx t)) <$> Text.lines t)
    indentIdx :: Text -> Maybe Int
    indentIdx = minimumMay . mapMaybe firstNonBlankChar . Text.lines
    firstNonBlankChar :: Text -> Maybe Int
    firstNonBlankChar = Text.findIndex (not . isSpace)

string :: MonadParsec e Text m => m Text
string = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

mkLoc :: Int -> SourcePos -> Loc
mkLoc offset SourcePos {..} = Loc {..}
  where
    _locFile = sourceName
    _locOffset = Pos (fromIntegral offset)
    _locFileLoc = FileLoc {..}
      where
        _locLine = fromPos sourceLine
        _locCol = fromPos sourceColumn

curLoc :: MonadParsec e Text m => m Loc
curLoc = do
  sp <- getSourcePos
  offset <- stateOffset <$> getParserState
  return (mkLoc offset sp)

interval :: MonadParsec e Text m => m a -> m (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

keyword :: Member InfoTableBuilder r => Text -> ParsecS r ()
keyword kw = do
  l <- snd <$> interval (symbol kw)
  lift (registerKeyword l)

-- | Same as @identifier@ but does not consume space after it.
-- TODO: revise.
bareIdentifier :: Member InfoTableBuilder r => ParsecS r (Text, Interval)
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

dottedIdentifier :: Member InfoTableBuilder r => ParsecS r (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

braces :: MonadParsec e Text m => m a -> m a
braces = between (symbol "{") (symbol "}")

allKeywords :: Member InfoTableBuilder r => [ParsecS r ()]
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

kwAssignment :: Member InfoTableBuilder r => ParsecS r ()
kwAssignment = keyword Str.assignUnicode <|> keyword Str.assignAscii

kwAxiom :: Member InfoTableBuilder r => ParsecS r ()
kwAxiom = keyword Str.axiom

-- | Note that the trailing space is needed to distinguish it from ':='.
kwColon :: Member InfoTableBuilder r => ParsecS r ()
kwColon = keyword Str.colonSpace

kwColonOmega :: Member InfoTableBuilder r => ParsecS r ()
kwColonOmega = keyword Str.colonOmegaUnicode <|> keyword Str.colonOmegaAscii

kwColonOne :: Member InfoTableBuilder r => ParsecS r ()
kwColonOne = keyword Str.colonOne

kwColonZero :: Member InfoTableBuilder r => ParsecS r ()
kwColonZero = keyword Str.colonZero

kwCompile :: Member InfoTableBuilder r => ParsecS r ()
kwCompile = keyword Str.compile

kwEnd :: Member InfoTableBuilder r => ParsecS r ()
kwEnd = keyword Str.end

kwEval :: Member InfoTableBuilder r => ParsecS r ()
kwEval = keyword Str.eval

kwHiding :: Member InfoTableBuilder r => ParsecS r ()
kwHiding = keyword Str.hiding

kwImport :: Member InfoTableBuilder r => ParsecS r ()
kwImport = keyword Str.import_

kwForeign :: Member InfoTableBuilder r => ParsecS r ()
kwForeign = keyword Str.foreign_

kwIn :: Member InfoTableBuilder r => ParsecS r ()
kwIn = keyword Str.in_

kwInductive :: Member InfoTableBuilder r => ParsecS r ()
kwInductive = keyword Str.inductive

kwInfix :: Member InfoTableBuilder r => ParsecS r ()
kwInfix = keyword Str.infix_

kwInfixl :: Member InfoTableBuilder r => ParsecS r ()
kwInfixl = keyword Str.infixl_

kwInfixr :: Member InfoTableBuilder r => ParsecS r ()
kwInfixr = keyword Str.infixr_

kwLambda :: Member InfoTableBuilder r => ParsecS r ()
kwLambda = keyword Str.lambdaUnicode <|> keyword Str.lambdaAscii

kwLet :: Member InfoTableBuilder r => ParsecS r ()
kwLet = keyword Str.let_

kwMapsTo :: Member InfoTableBuilder r => ParsecS r ()
kwMapsTo = keyword Str.mapstoUnicode <|> keyword Str.mapstoAscii

kwMatch :: Member InfoTableBuilder r => ParsecS r ()
kwMatch = keyword Str.match

kwModule :: Member InfoTableBuilder r => ParsecS r ()
kwModule = keyword Str.module_

kwOpen :: Member InfoTableBuilder r => ParsecS r ()
kwOpen = keyword Str.open

kwPostfix :: Member InfoTableBuilder r => ParsecS r ()
kwPostfix = keyword Str.postfix

kwPrint :: Member InfoTableBuilder r => ParsecS r ()
kwPrint = keyword Str.print

kwPublic :: Member InfoTableBuilder r => ParsecS r ()
kwPublic = keyword Str.public

kwRightArrow :: Member InfoTableBuilder r => ParsecS r ()
kwRightArrow = keyword Str.toUnicode <|> keyword Str.toAscii

kwSemicolon :: Member InfoTableBuilder r => ParsecS r ()
kwSemicolon = keyword Str.semicolon

kwType :: Member InfoTableBuilder r => ParsecS r ()
kwType = keyword Str.type_

kwTerminating :: Member InfoTableBuilder r => ParsecS r ()
kwTerminating = keyword Str.terminating

kwUsing :: Member InfoTableBuilder r => ParsecS r ()
kwUsing = keyword Str.using

kwWhere :: Member InfoTableBuilder r => ParsecS r ()
kwWhere = keyword Str.where_

kwWildcard :: Member InfoTableBuilder r => ParsecS r ()
kwWildcard = keyword Str.underscore

ghc :: Member InfoTableBuilder r => ParsecS r ()
ghc = keyword Str.ghc

agda :: Member InfoTableBuilder r => ParsecS r ()
agda = keyword Str.agda
