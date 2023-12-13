module Juvix.Compiler.Nockma.Translation.FromSource where

import Juvix.Compiler.Nockma.Language qualified as N
import Juvix.Parser.Error
import Juvix.Prelude hiding (Atom, many, some)
import Juvix.Prelude.Parsing hiding (runParser)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseText :: Text -> Either MegaparsecError (N.Term Natural)
parseText = runParser ""

runParser :: FilePath -> Text -> Either MegaparsecError (N.Term Natural)
runParser f input = case P.runParser topTerm f input of
  Left err -> Left (MegaparsecError err)
  Right t -> Right t

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
  where
    spaceConsumer :: Parser ()
    spaceConsumer = L.space space1 empty empty

lsbracket :: Parser ()
lsbracket = void (lexeme "[")

rsbracket :: Parser ()
rsbracket = void (lexeme "]")

natural :: Parser Natural
natural = lexeme L.decimal

atom :: Parser (N.Term Natural)
atom = N.TermAtom . N.Atom <$> natural

cell :: Parser (N.Term Natural)
cell = do
  lsbracket
  firstTerm <- term
  restTerms <- some term
  rsbracket
  return (buildCell firstTerm restTerms)
  where
    buildCell :: N.Term Natural -> NonEmpty (N.Term Natural) -> N.Term Natural
    buildCell h = \case
      x :| [] -> N.TermCell (N.Cell h x)
      y :| (w : ws) -> N.TermCell (N.Cell h (buildCell y (w :| ws)))

term :: Parser (N.Term Natural)
term = atom <|> cell

topTerm :: Parser (N.Term Natural)
topTerm = cell
