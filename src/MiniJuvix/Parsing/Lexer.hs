module MiniJuvix.Parsing.Lexer where


import MiniJuvix.Utils.Prelude
import Text.Megaparsec            as M
import Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.Unicode

type OperatorSym = Text

space ∷ ∀ m e. MonadParsec e Text m ⇒ m ()
space = L.space space1 lineComment block
  where
    lineComment = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme ∷ MonadParsec e Text m ⇒ m a → m a
lexeme = L.lexeme space

symbol ∷ MonadParsec e Text m ⇒ Text → m ()
symbol = void . L.symbol space

decimal ∷ (MonadParsec e Text m, Num n) ⇒ m n
decimal = lexeme L.decimal

identifier ∷ MonadParsec e Text m ⇒ m Text
identifier = do
  notFollowedBy (choice allKeywords)
  lexeme $
    M.takeWhileP Nothing (isAlphaNum .||. (`elem`("_'-" ∷ String)))

allKeywords ∷ MonadParsec e Text m ⇒ [m ()]
allKeywords =
  [
    kwArrowR
  , kwCase
  , kwColon
  , kwEnd
  , kwHiding
  , kwImport
  , kwInductive
  , kwInfix
  , kwInfixl
  , kwInfixr
  , kwLambda
  , kwLet
  , kwMapsTo
  , kwModule
  , kwOpen
  , kwPostfix
  , kwPrefix
  , kwSemicolon
  , kwType
  , kwWhere
  , kwOmega
  ]

parens ∷ MonadParsec e Text m ⇒ m a → m a
parens = between (symbol "(") (symbol ")")

braces ∷ MonadParsec e Text m ⇒ m a → m a
braces = between (symbol "{") (symbol "}")

kwLet ∷ MonadParsec e Text m ⇒ m ()
kwLet = symbol "let"

kwInductive ∷ MonadParsec e Text m ⇒ m ()
kwInductive = symbol "inductive"

kwSemicolon ∷ MonadParsec e Text m ⇒ m ()
kwSemicolon = symbol ";"

kwDef ∷ MonadParsec e Text m ⇒ m ()
kwDef = symbol "≔" <|> symbol ":="

kwColon ∷ MonadParsec e Text m ⇒ m ()
kwColon = symbol ":"

kwArrowR ∷ MonadParsec e Text m ⇒ m ()
kwArrowR = symbol "→" <|> symbol "->"

kwMapsTo ∷ MonadParsec e Text m ⇒ m ()
kwMapsTo = symbol "↦" <|> symbol "->"

kwLambda ∷ MonadParsec e Text m ⇒ m ()
kwLambda = symbol "λ" <|> symbol "\\"

kwCase ∷ MonadParsec e Text m ⇒ m ()
kwCase = symbol "case"

kwType ∷ MonadParsec e Text m ⇒ m ()
kwType = symbol "Type"

kwInfix ∷ MonadParsec e Text m ⇒ m ()
kwInfix = symbol "infix"

kwInfixr ∷ MonadParsec e Text m ⇒ m ()
kwInfixr = symbol "infixr"

kwInfixl ∷ MonadParsec e Text m ⇒ m ()
kwInfixl = symbol "infixl"

kwPostfix ∷ MonadParsec e Text m ⇒ m ()
kwPostfix = symbol "postfix"

kwPrefix ∷ MonadParsec e Text m ⇒ m ()
kwPrefix = symbol "prefix"

kwOpen ∷ MonadParsec e Text m ⇒ m ()
kwOpen = symbol "open"

kwImport ∷ MonadParsec e Text m ⇒ m ()
kwImport = symbol "import"

kwHiding ∷ MonadParsec e Text m ⇒ m ()
kwHiding = symbol "hiding"

kwModule ∷ MonadParsec e Text m ⇒ m ()
kwModule = symbol "module"

kwEnd ∷ MonadParsec e Text m ⇒ m ()
kwEnd = symbol "end"

kwWhere ∷ MonadParsec e Text m ⇒ m ()
kwWhere = symbol "where"

kwOmega ∷ MonadParsec e Text m ⇒ m ()
kwOmega = symbol "ω" <|> symbol "any"
