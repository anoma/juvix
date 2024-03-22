module Juvix.Compiler.Nockma.Translation.FromSource.Base where

import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Nockma.Language
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Error
import Juvix.Parser.Lexer (isWhiteSpace, onlyInterval, withLoc)
import Juvix.Prelude hiding (Atom, Path, many, some)
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Parsing hiding (runParser)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

parseText :: Text -> Either MegaparsecError (Term Natural)
parseText = runParser noFile

parseReplText :: Text -> Either MegaparsecError (ReplTerm Natural)
parseReplText = runParserFor replTerm noFile

parseTermFile :: (MonadIO m) => Prelude.Path Abs File -> m (Either MegaparsecError (Term Natural))
parseTermFile fp = do
  txt <- readFile fp
  return (runParser fp txt)

parseProgramFile :: (MonadIO m) => Prelude.Path Abs File -> m (Either MegaparsecError (Program Natural))
parseProgramFile fp = do
  txt <- readFile fp
  return (runParserProgram fp txt)

parseReplStatement :: Text -> Either MegaparsecError (ReplStatement Natural)
parseReplStatement = runParserFor replStatement noFile

runParserProgram :: Prelude.Path Abs File -> Text -> Either MegaparsecError (Program Natural)
runParserProgram = runParserFor program

runParserFor :: Parser a -> Prelude.Path Abs File -> Text -> Either MegaparsecError a
runParserFor p f input_ = case P.runParser (spaceConsumer >> p <* eof) (toFilePath f) input_ of
  Left err -> Left (MegaparsecError err)
  Right t -> Right t

runParser :: Prelude.Path Abs File -> Text -> Either MegaparsecError (Term Natural)
runParser = runParserFor term

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment empty
  where
    lineComment :: Parser () = L.skipLineComment "--"

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

atomOp :: Maybe Tag -> Parser (Atom Natural)
atomOp mtag = do
  WithLoc loc op' <- withLoc (choice [symbol opName $> op | (opName, op) <- HashMap.toList atomOps])
  let info =
        AtomInfo
          { _atomInfoHint = Just AtomHintOp,
            _atomInfoTag = mtag,
            _atomInfoLoc = Irrelevant (Just loc)
          }
  return (Atom (serializeNockOp op') info)

atomPath :: Maybe Tag -> Parser (Atom Natural)
atomPath mtag = do
  WithLoc loc path <- withLoc pPath
  let info =
        AtomInfo
          { _atomInfoHint = Just AtomHintPath,
            _atomInfoTag = mtag,
            _atomInfoLoc = Irrelevant (Just loc)
          }
  return (Atom (serializePath path) info)

direction :: Parser Direction
direction =
  symbol "L" $> L
    <|> symbol "R" $> R

pPath :: Parser Path
pPath =
  symbol "S" $> []
    <|> NonEmpty.toList <$> some direction

atomNat :: Maybe Tag -> Parser (Atom Natural)
atomNat mtag = do
  WithLoc loc n <- withLoc dottedNatural
  let info =
        AtomInfo
          { _atomInfoHint = Nothing,
            _atomInfoTag = mtag,
            _atomInfoLoc = Irrelevant (Just loc)
          }
  return (Atom n info)

atomBool :: Parser (Atom Natural)
atomBool =
  choice
    [ symbol Str.true $> nockTrue,
      symbol Str.false $> nockFalse
    ]

atomWithLoc :: Parser a -> Atom Natural -> Parser (Atom Natural)
atomWithLoc p n = do
  loc <- onlyInterval p
  return (set atomLoc (Just loc) n)

atomNil :: Parser (Atom Natural)
atomNil = symbol Str.nil $> nockNil

atomVoid :: Parser (Atom Natural)
atomVoid = symbol Str.void $> nockVoid

patom :: Parser (Atom Natural)
patom = do
  mtag <- optional pTag
  atomOp mtag
    <|> atomNat mtag
    <|> atomPath mtag
    <|> atomBool
    <|> atomNil
    <|> atomVoid

iden :: Parser Text
iden = lexeme (takeWhile1P (Just "<iden>") (isAscii .&&. not . isWhiteSpace))

pTag :: Parser Tag
pTag = do
  void (chunk Str.tagTag)
  Tag <$> iden

cell :: Parser (Cell Natural)
cell = do
  lloc <- onlyInterval lsbracket
  lbl <- optional pTag
  c <- optional stdlibCall
  firstTerm <- term
  restTerms <- some term
  rloc <- onlyInterval rsbracket
  let r = buildCell firstTerm restTerms
      info =
        CellInfo
          { _cellInfoCall = c,
            _cellInfoTag = lbl,
            _cellInfoLoc = Irrelevant (Just (lloc <> rloc))
          }
  return (set cellInfo info r)
  where
    stdlibCall :: Parser (StdlibCall Natural)
    stdlibCall = do
      chunk Str.stdlibTag
      f <- stdlibFun
      chunk Str.argsTag
      args <- term
      return
        StdlibCall
          { _stdlibCallArgs = args,
            _stdlibCallFunction = f
          }

    stdlibFun :: Parser StdlibFunction
    stdlibFun = do
      i <- iden
      let err = error ("invalid stdlib function identifier: " <> i)
      maybe err return (parseStdlibFunction i)

    buildCell :: Term Natural -> NonEmpty (Term Natural) -> Cell Natural
    buildCell h = \case
      x :| [] -> Cell h x
      y :| (w : ws) -> Cell h (TermCell (buildCell y (w :| ws)))

term :: Parser (Term Natural)
term =
  TermAtom <$> patom
    <|> TermCell <$> cell

assig :: Parser (Assignment Natural)
assig = do
  n <- name
  symbol ":="
  t <- term
  return
    Assignment
      { _assignmentName = n,
        _assignmentBody = t
      }

program :: Parser (Program Natural)
program = Program <$> many statement <* eof
  where
    statement :: Parser (Statement Natural)
    statement =
      P.try (StatementAssignment <$> assig)
        <|> StatementStandalone <$> term

name :: Parser Text
name = lexeme $ do
  h <- P.satisfy isLetter
  hs <- P.takeWhileP Nothing isAlphaNum
  return (Text.cons h hs)

withStack :: Parser (WithStack Natural)
withStack = do
  st <- replTerm
  symbol "/"
  tm <- replTerm
  return
    WithStack
      { _withStackStack = st,
        _withStackTerm = tm
      }

replExpression :: Parser (ReplExpression Natural)
replExpression =
  ReplExpressionWithStack <$> P.try withStack
    <|> ReplExpressionTerm <$> replTerm

replStatement :: Parser (ReplStatement Natural)
replStatement =
  ReplStatementAssignment <$> P.try assig
    <|> ReplStatementExpression <$> replExpression

replTerm :: Parser (ReplTerm Natural)
replTerm =
  ReplName <$> name
    <|> ReplTerm <$> term
