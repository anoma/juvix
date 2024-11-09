module Juvix.Compiler.Nockma.Translation.FromSource.Base
  ( module Juvix.Compiler.Nockma.Translation.FromSource.Base,
    module Juvix.Compiler.Nockma.Highlight.Input,
  )
where

import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Nockma.Encoding.ByteString (textToNatural)
import Juvix.Compiler.Nockma.Encoding.Cue qualified as Cue
import Juvix.Compiler.Nockma.Highlight.Input
import Juvix.Compiler.Nockma.Language
-- import Juvix.Data.CodeAnn

import Juvix.Data.CodeAnn (AnsiText, mkAnsiText, ppCodeAnn)
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Error
import Juvix.Parser.Lexer (isWhiteSpace, onlyInterval, withLoc)
import Juvix.Prelude hiding (Atom, Path, many, some)
import Juvix.Prelude qualified as Prelude
import Juvix.Prelude.Parsing hiding (runParser)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

type ParserSem = '[HighlightBuilder]

type Parser = ParsecT Void Text (Sem '[HighlightBuilder])

parseText :: Text -> Either MegaparsecError (Term Natural)
parseText = runParser noFile

parseReplText :: Text -> Either MegaparsecError (ReplTerm Natural)
parseReplText = runParserFor replTerm noFile

-- | If the file ends in .debug.nockma it parses an annotated unjammed term. Otherwise
-- it is equivalent to cueJammedFile
cueJammedFileOrPretty ::
  forall r.
  (Members '[Files, Error JuvixError] r) =>
  Prelude.Path Abs File ->
  Sem r (Term Natural)
cueJammedFileOrPretty f
  | f `hasExtensions` nockmaDebugFileExts = ignoreHighlightBuilder (parseTermFile f)
  | otherwise = cueJammedFile f

-- | If the file ends in .debug.nockma it parses an annotated unjammed program. Otherwise
-- it parses program with a single jammed term
cueJammedFileOrPrettyProgram ::
  forall r.
  (Members '[Files, Error JuvixError] r) =>
  Prelude.Path Abs File ->
  Sem r (Program Natural)
cueJammedFileOrPrettyProgram f
  | f `hasExtensions` nockmaDebugFileExts = parseProgramFile f
  | otherwise = singletonProgram <$> cueJammedFile f

cueJammedFile :: forall r. (Members '[Files, Error JuvixError] r) => Prelude.Path Abs File -> Sem r (Term Natural)
cueJammedFile fp = do
  bs <- readFileBS' fp
  case Cue.cueFromByteString'' @Natural bs of
    Left e -> natErr e
    Right (Left e) -> decodingErr e
    Right (Right t) -> return t
  where
    err :: AnsiText -> Sem r x
    err msg =
      throw $
        JuvixError
          GenericError
            { _genericErrorLoc = i,
              _genericErrorIntervals = [i],
              _genericErrorMessage = msg
            }

    decodingErr :: Cue.DecodingError -> Sem r x
    decodingErr e = err (mkAnsiText (ppCodeAnn e))

    natErr :: NockNaturalNaturalError -> Sem r x
    natErr e = err (mkAnsiText (ppCodeAnn e))

    i :: Interval
    i = mkInterval loc loc
      where
        loc :: Loc
        loc = mkInitialLoc fp

parseTermFile :: (Members '[Files, Error JuvixError, HighlightBuilder] r) => Prelude.Path Abs File -> Sem r (Term Natural)
parseTermFile fp = do
  txt <- readFile' fp
  mapError (JuvixError @MegaparsecError) $
    runParserForSem term fp txt

parseProgramFile :: (Members '[Files, Error JuvixError] r) => Prelude.Path Abs File -> Sem r (Program Natural)
parseProgramFile fp = do
  txt <- readFile' fp
  either (throw . JuvixError) return (runParserProgram fp txt)

parseReplStatement :: Text -> Either MegaparsecError (ReplStatement Natural)
parseReplStatement = runParserFor replStatement noFile

runParserProgram :: Prelude.Path Abs File -> Text -> Either MegaparsecError (Program Natural)
runParserProgram = runParserFor program

runParserForSem ::
  (Members '[Error MegaparsecError, HighlightBuilder] r) =>
  Parser a ->
  Prelude.Path Abs File ->
  Text ->
  Sem r a
runParserForSem p f txt = do
  x <- inject (P.runParserT (spaceConsumer >> p <* eof) (toFilePath f) txt)
  case x of
    Left err -> throw (MegaparsecError err)
    Right t -> return t

runParserFor :: Parser a -> Prelude.Path Abs File -> Text -> Either MegaparsecError a
runParserFor p f = run . ignoreHighlightBuilder . runError . runParserForSem p f

runParser :: Prelude.Path Abs File -> Text -> Either MegaparsecError (Term Natural)
runParser = runParserFor term

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment empty
  where
    lineComment :: Parser () = L.skipLineComment Str.commentLineStart

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parsedItem :: ParsedItemTag -> Parser a -> Parser a
parsedItem t p = do
  WithLoc {..} <- withLoc p
  lift $
    highlightItem
      ParsedItem
        { _parsedLoc = _withLocInt,
          _parsedTag = t
        }
  return _withLocParam

delimiter :: Text -> Parser ()
delimiter = parsedItem ParsedTagDelimiter . void . lexeme . chunk

lsbracket :: Parser ()
lsbracket = delimiter "["

rsbracket :: Parser ()
rsbracket = delimiter "]"

stringLiteral :: Parser Text
stringLiteral = lexeme (pack <$> (char '"' >> manyTill L.charLiteral (char '"')))

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
atomNil = choice (map symbol [Str.nil, Str.functionsPlaceholder, Str.stdlibPlaceholder]) $> nockNil

atomVoid :: Parser (Atom Natural)
atomVoid = symbol Str.void $> nockVoid

atomStringLiteral :: Parser (Atom Natural)
atomStringLiteral = do
  WithLoc loc s <- withLoc stringLiteral
  let info =
        AtomInfo
          { _atomInfoTag = Nothing,
            _atomInfoLoc = Irrelevant (Just loc),
            _atomInfoHint = Just AtomHintString
          }
  return (Atom (textToNatural s) info)

atomNockHint :: Maybe Tag -> Parser (Atom Natural)
atomNockHint mtag = do
  symbol Str.percent
  let hints :: [NockHint] = enumerate
  val <- choice (map (\hnt -> symbol (nockHintName hnt) >> return (nockHintValue hnt)) hints)
  return (Atom val emptyAtomInfo {_atomInfoTag = mtag})

patom :: Parser (Atom Natural)
patom = do
  mtag <- optional pTag
  atomOp mtag
    <|> atomNockHint mtag
    <|> atomNat mtag
    <|> atomPath mtag
    <|> atomBool
    <|> atomNil
    <|> atomVoid
    <|> try atomStringLiteral

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
  c <- optional anomaLibCall
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
    anomaLibCall :: Parser (AnomaLibCall Natural)
    anomaLibCall = do
      chunk Str.stdlibTag
      f <- stdlibFun
      chunk Str.argsTag
      args <- term
      return
        AnomaLibCall
          { _anomaLibCallArgs = args,
            _anomaLibCallRef = f
          }

    stdlibFun :: Parser AnomaLib
    stdlibFun = do
      i <- iden
      let err = error ("invalid stdlib function identifier: " <> i)
      maybe err return (parseAnomaLib i)

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
