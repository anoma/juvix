module MiniJuvix.Syntax.Concrete.Lexer where

--------------------------------------------------------------------------------

import qualified Data.Text as Text
import GHC.Unicode
import qualified MiniJuvix.Internal.Strings as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base hiding (Pos, space)
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Loc
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------

type OperatorSym = Text

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

identifier :: MonadParsec e Text m => m Text
identifier = fmap fst identifierL

identifierL :: MonadParsec e Text m => m (Text, Interval)
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
  Text.strip . pack <$> (char '{' >> manyTill anySingle (char '}'))

string :: MonadParsec e Text m => m Text
string = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

mkLoc :: SourcePos -> Loc
mkLoc SourcePos {..} = Loc {..}
  where
    _locFile = sourceName
    _locFileLoc = FileLoc {..}
      where
        _locLine = fromPos sourceLine
        _locCol = fromPos sourceColumn

curLoc :: MonadParsec e Text m => m Loc
curLoc = mkLoc <$> getSourcePos

withLoc :: MonadParsec e Text m => (Loc -> m a) -> m a
withLoc ma = curLoc >>= ma

interval :: MonadParsec e Text m => m a -> m (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

-- | Same as @identifier@ but does not consume space after it.
-- TODO: revise.
bareIdentifier :: MonadParsec e Text m => m (Text, Interval)
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

dottedIdentifier :: forall e m. MonadParsec e Text m => m (NonEmpty (Text, Interval))
dottedIdentifier = lexeme $ P.sepBy1 bareIdentifier dot

braces :: MonadParsec e Text m => m a -> m a
braces = between (symbol "{") (symbol "}")

allKeywords :: MonadParsec e Text m => [m ()]
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

kwAssignment :: MonadParsec e Text m => m ()
kwAssignment = symbol Str.assignUnicode <|> symbol Str.assignAscii

kwAxiom :: MonadParsec e Text m => m ()
kwAxiom = symbol Str.axiom

-- | Note that the trailing space is needed to distinguish it from ':='.
kwColon :: MonadParsec e Text m => m ()
kwColon = symbol Str.colonSpace

kwColonOmega :: MonadParsec e Text m => m ()
kwColonOmega = symbol Str.colonOmegaUnicode <|> symbol Str.colonOmegaAscii

kwColonOne :: MonadParsec e Text m => m ()
kwColonOne = symbol Str.colonOne

kwColonZero :: MonadParsec e Text m => m ()
kwColonZero = symbol Str.colonZero

kwEnd :: MonadParsec e Text m => m ()
kwEnd = symbol Str.end

kwEval :: MonadParsec e Text m => m ()
kwEval = symbol Str.eval

kwHiding :: MonadParsec e Text m => m ()
kwHiding = symbol Str.hiding

kwImport :: MonadParsec e Text m => m ()
kwImport = symbol Str.import_

kwForeign :: MonadParsec e Text m => m ()
kwForeign = symbol Str.foreign_

kwCompile :: MonadParsec e Text m => m ()
kwCompile = symbol Str.compile

kwIn :: MonadParsec e Text m => m ()
kwIn = symbol Str.in_

kwInductive :: MonadParsec e Text m => m ()
kwInductive = symbol Str.inductive

kwInfix :: MonadParsec e Text m => m ()
kwInfix = symbol Str.infix_

kwInfixl :: MonadParsec e Text m => m ()
kwInfixl = symbol Str.infixl_

kwInfixr :: MonadParsec e Text m => m ()
kwInfixr = symbol Str.infixr_

kwLambda :: MonadParsec e Text m => m ()
kwLambda = symbol Str.lambdaUnicode <|> symbol Str.lambdaAscii

kwLet :: MonadParsec e Text m => m ()
kwLet = symbol Str.let_

kwMapsTo :: MonadParsec e Text m => m ()
kwMapsTo = symbol Str.mapstoUnicode <|> symbol Str.mapstoAscii

kwMatch :: MonadParsec e Text m => m ()
kwMatch = symbol Str.match

kwModule :: MonadParsec e Text m => m ()
kwModule = symbol Str.module_

kwOpen :: MonadParsec e Text m => m ()
kwOpen = symbol Str.open

kwPostfix :: MonadParsec e Text m => m ()
kwPostfix = symbol Str.postfix

kwPrint :: MonadParsec e Text m => m ()
kwPrint = symbol Str.print

kwPublic :: MonadParsec e Text m => m ()
kwPublic = symbol Str.public

kwRightArrow :: MonadParsec e Text m => m ()
kwRightArrow = symbol Str.toUnicode <|> symbol Str.toAscii

kwSemicolon :: MonadParsec e Text m => m ()
kwSemicolon = symbol Str.semicolon

kwType :: MonadParsec e Text m => m ()
kwType = symbol Str.type_

kwUsing :: MonadParsec e Text m => m ()
kwUsing = symbol Str.using

kwWhere :: MonadParsec e Text m => m ()
kwWhere = symbol Str.where_

kwWildcard :: MonadParsec e Text m => m ()
kwWildcard = symbol Str.underscore

ghc :: MonadParsec e Text m => m ()
ghc = symbol Str.ghc
