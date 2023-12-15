module Juvix.Compiler.Nockma.Translation.FromSource where

import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
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
runParser f input = case P.runParser term f input of
  Left err -> Left (MegaparsecError err)
  Right t -> Right t

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lsbracket :: Parser ()
lsbracket = void (lexeme "[")

rsbracket :: Parser ()
rsbracket = void (lexeme "]")

dottedNatural :: Parser Natural
dottedNatural = lexeme $ do
  firstDigit <- digit
  rest <- many (digit <|> dotAndDigit)
  return (foldl' (\acc n -> acc * 10 + fromIntegral (digitToInt n)) 0 (firstDigit : rest))
  where
    dotAndDigit :: Parser Char
    dotAndDigit = char '.' *> satisfy isDigit

    digit :: Parser Char
    digit = satisfy isDigit

atomOp :: Parser (N.Atom Natural)
atomOp = do
  op' <- choice [symbol opName $> op | (opName, op) <- HashMap.toList N.atomOps]
  return (N.Atom (N.serializeNockOp op') (Irrelevant (Just N.AtomHintOp)))

atomDirection :: Parser (N.Atom Natural)
atomDirection = do
  dirs <-
    symbol "S" $> []
      <|> NonEmpty.toList <$> some (choice [symbol "L" $> N.L, symbol "R" $> N.R])
  return (N.Atom (N.serializePosition (N.Position dirs)) (Irrelevant (Just N.AtomHintPosition)))

atomNat :: Parser (N.Atom Natural)
atomNat = (\n -> N.Atom n (Irrelevant Nothing)) <$> dottedNatural

atomBool :: Parser (N.Atom Natural)
atomBool = choice [symbol "true" $> N.nockTrue, symbol "false" $> N.nockFalse]

atom :: Parser (N.Atom Natural)
atom = atomOp <|> atomNat <|> atomDirection <|> atomBool

cell :: Parser (N.Cell Natural)
cell = do
  lsbracket
  firstTerm <- term
  restTerms <- some term
  rsbracket
  return (buildCell firstTerm restTerms)
  where
    buildCell :: N.Term Natural -> NonEmpty (N.Term Natural) -> N.Cell Natural
    buildCell h = \case
      x :| [] -> N.Cell h x
      y :| (w : ws) -> N.Cell h (N.TermCell (buildCell y (w :| ws)))

term :: Parser (N.Term Natural)
term =
  N.TermAtom <$> atom
    <|> N.TermCell <$> cell
