module MiniJuvix.Syntax.Concrete.Lexer where

--------------------------------------------------------------------------------

import GHC.Unicode
import MiniJuvix.Syntax.Concrete.Base hiding (space)
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Utils.Prelude
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
identifier = lexeme bareIdentifier

allowedSymbols :: String
allowedSymbols = "_'-"

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: MonadParsec e Text m => m Text
bareIdentifier = do
  notFollowedBy (choice allKeywords)
  P.takeWhile1P Nothing validChar
  where
    validChar :: Char -> Bool
    validChar c =
      or
        [ isAlphaNum c,
          isMathSymbol,
          isCurrencySymbol,
          c `elem` ("_'-" :: String)
        ]
      where
        cat = generalCategory c
        isMathSymbol = cat == MathSymbol
        isCurrencySymbol = cat == CurrencySymbol

dot :: forall e m. MonadParsec e Text m => m Char
dot = P.char '.'

dottedIdentifier :: forall e m. MonadParsec e Text m => m (NonEmpty Text)
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
    kwEnd,
    kwEval,
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
    kwPrefix,
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
kwAssignment = symbol "≔" <|> symbol ":="

kwAxiom :: MonadParsec e Text m => m ()
kwAxiom = symbol "axiom"

-- | Note that the trailing space is needed to distinguish it from ':='.
kwColon :: MonadParsec e Text m => m ()
kwColon = symbol ": "

kwColonOmega :: MonadParsec e Text m => m ()
kwColonOmega = symbol ":ω" <|> symbol ":any"

kwColonOne :: MonadParsec e Text m => m ()
kwColonOne = symbol ":1"

kwColonZero :: MonadParsec e Text m => m ()
kwColonZero = symbol ":0"

kwEnd :: MonadParsec e Text m => m ()
kwEnd = symbol "end"

kwEval :: MonadParsec e Text m => m ()
kwEval = symbol "eval"

kwHiding :: MonadParsec e Text m => m ()
kwHiding = symbol "hiding"

kwImport :: MonadParsec e Text m => m ()
kwImport = symbol "import"

kwIn :: MonadParsec e Text m => m ()
kwIn = symbol "in"

kwInductive :: MonadParsec e Text m => m ()
kwInductive = symbol "inductive"

kwInfix :: MonadParsec e Text m => m ()
kwInfix = symbol "infix"

kwInfixl :: MonadParsec e Text m => m ()
kwInfixl = symbol "infixl"

kwInfixr :: MonadParsec e Text m => m ()
kwInfixr = symbol "infixr"

kwLambda :: MonadParsec e Text m => m ()
kwLambda = symbol "λ" <|> symbol "\\"

kwLet :: MonadParsec e Text m => m ()
kwLet = symbol "let"

kwMapsTo :: MonadParsec e Text m => m ()
kwMapsTo = symbol "↦" <|> symbol "->"

kwMatch :: MonadParsec e Text m => m ()
kwMatch = symbol "match"

kwModule :: MonadParsec e Text m => m ()
kwModule = symbol "module"

kwOpen :: MonadParsec e Text m => m ()
kwOpen = symbol "open"

kwPostfix :: MonadParsec e Text m => m ()
kwPostfix = symbol "postfix"

kwPrefix :: MonadParsec e Text m => m ()
kwPrefix = symbol "prefix"

kwPrint :: MonadParsec e Text m => m ()
kwPrint = symbol "print"

kwPublic :: MonadParsec e Text m => m ()
kwPublic = symbol "public"

kwRightArrow :: MonadParsec e Text m => m ()
kwRightArrow = symbol "→" <|> symbol "->"

kwSemicolon :: MonadParsec e Text m => m ()
kwSemicolon = symbol ";"

kwType :: MonadParsec e Text m => m ()
kwType = symbol "Type"

kwUsing :: MonadParsec e Text m => m ()
kwUsing = symbol "using"

kwWhere :: MonadParsec e Text m => m ()
kwWhere = symbol "where"

kwWildcard :: MonadParsec e Text m => m ()
kwWildcard = symbol "_"
