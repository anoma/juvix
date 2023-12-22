module Juvix.Compiler.Nockma.Translation.FromSource where

import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Nockma.Language qualified as N
import Juvix.Compiler.Nockma.StdlibSrc (stdlibSrc)
import Juvix.Parser.Error
import Juvix.Prelude hiding (Atom, many, some)
import Juvix.Prelude.Parsing hiding (runParser)
import Juvix.Prelude.Pretty
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

fromMegaParsecError :: Either MegaparsecError a -> a
fromMegaParsecError = \case
  Left e -> error (prettyText e)
  Right a -> a

stdlib :: N.Term Natural
stdlib = fromMegaParsecError (parseText stdlibSrc)

parseText :: Text -> Either MegaparsecError (N.Term Natural)
parseText = runParser ""

parseReplText :: Text -> Either MegaparsecError (N.ReplTerm Natural)
parseReplText = runParserFor replTerm ""

parseProgramFile :: (MonadIO m) => FilePath -> m (Either MegaparsecError (N.Program Natural))
parseProgramFile fp = do
  txt <- readFile fp
  return (runParserProgram fp txt)

parseReplStatement :: Text -> Either MegaparsecError (N.ReplStatement Natural)
parseReplStatement = runParserFor replStatement ""

runParserProgram :: FilePath -> Text -> Either MegaparsecError (N.Program Natural)
runParserProgram = runParserFor program

runParserFor :: Parser a -> FilePath -> Text -> Either MegaparsecError a
runParserFor p f input = case P.runParser (spaceConsumer >> p <* eof) f input of
  Left err -> Left (MegaparsecError err)
  Right t -> Right t

runParser :: FilePath -> Text -> Either MegaparsecError (N.Term Natural)
runParser = runParserFor term

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
  return (N.Atom (N.serializePath dirs) (Irrelevant (Just N.AtomHintPath)))

atomNat :: Parser (N.Atom Natural)
atomNat = (\n -> N.Atom n (Irrelevant Nothing)) <$> dottedNatural

atomBool :: Parser (N.Atom Natural)
atomBool =
  choice
    [ symbol "true" $> N.nockTrue,
      symbol "false" $> N.nockFalse
    ]

atomNil :: Parser (N.Atom Natural)
atomNil = symbol "nil" $> N.nockNil

atom :: Parser (N.Atom Natural)
atom =
  atomOp
    <|> atomNat
    <|> atomDirection
    <|> atomBool
    <|> atomNil

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

assig :: Parser (N.Assignment Natural)
assig = do
  n <- name
  symbol ":="
  t <- term
  return
    N.Assignment
      { _assignmentName = n,
        _assignmentBody = t
      }

program :: Parser (N.Program Natural)
program = N.Program <$> many statement <* eof
  where
    statement :: Parser (N.Statement Natural)
    statement =
      P.try (N.StatementAssignment <$> assig)
        <|> N.StatementStandalone <$> term

name :: Parser Text
name = lexeme $ do
  h <- P.satisfy isLetter
  hs <- P.takeWhileP Nothing isAlphaNum
  return (Text.cons h hs)

withStack :: Parser (N.WithStack Natural)
withStack = do
  st <- replTerm
  symbol "/"
  tm <- replTerm
  return
    N.WithStack
      { _withStackStack = st,
        _withStackTerm = tm
      }

replExpression :: Parser (N.ReplExpression Natural)
replExpression =
  N.ReplExpressionWithStack <$> P.try withStack
    <|> N.ReplExpressionTerm <$> replTerm

replStatement :: Parser (N.ReplStatement Natural)
replStatement =
  N.ReplStatementAssignment <$> P.try assig
    <|> N.ReplStatementExpression <$> replExpression

replTerm :: Parser (N.ReplTerm Natural)
replTerm =
  N.ReplName <$> name
    <|> N.ReplTerm <$> term
