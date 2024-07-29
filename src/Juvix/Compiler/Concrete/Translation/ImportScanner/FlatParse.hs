module Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
    scanBSImports,
  )
where

import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.FlatParse hiding (Pos)
import Juvix.Prelude.FlatParse qualified as FP
import Juvix.Prelude.FlatParse.Lexer qualified as L

scanBSImports :: Path Abs File -> ByteString -> Maybe ScanResult
scanBSImports fp
  | isJuvixFile fp = fromResult . scanner fp
  | otherwise = const Nothing
  where
    fromResult :: Result () ok -> Maybe ok
    fromResult = \case
      OK r _ -> Just r
      _ -> Nothing

lexeme :: Parser e a -> Parser e a
lexeme p = p <* whiteSpaceAndComments

whiteSpaceAndComments :: Parser e ()
whiteSpaceAndComments = skipMany (L.whiteSpace1 <|> comment)

-- | The input is a utf-8 encoded bytestring
scanner :: Path Abs File -> ByteString -> Result e ScanResult
scanner fp bs = do
  spansToLocs <$> runParser pPreScanResult bs
  where
    getInterval :: (Members '[Input FileLoc] r) => Sem r Interval
    getInterval = do
      _intervalStart <- inputJust
      _intervalEnd <- inputJust
      return
        Interval
          { _intervalFile = fp,
            ..
          }

    spansToLocs :: [ImportScanParsed] -> ScanResult
    spansToLocs imports = run . runInputList allFileLocs $ do
      _scanResultImports <- fmap hashSet . forM imports $ \(imp :: ImportScanParsed) -> do
        loc <- getInterval
        return (set importLoc loc imp)
      return ScanResult {..}
      where
        allFileLocs :: [FileLoc]
        allFileLocs =
          [ FileLoc
              { _locLine = Pos (fromIntegral l),
                _locCol = Pos (fromIntegral c),
                _locOffset = Pos (fromIntegral p)
              }
            | (FP.Pos p, (l, c)) <- zipExact importsPositions (posLineCols bs importsPositions)
          ]

        importsPositions :: [FP.Pos]
        importsPositions = concatMap spanToPos importsSpans
          where
            importsSpans :: [Span]
            importsSpans = map (^. importLoc) imports

    spanToPos :: Span -> [FP.Pos]
    spanToPos (Span l r) = [l, r]

pPreScanResult :: Parser e [ImportScanParsed]
pPreScanResult = do
  whiteSpaceAndComments
  imports <- mapMaybe getImport <$> many pToken
  eof
  return imports
  where
    getImport :: Token -> Maybe ImportScanParsed
    getImport = \case
      TokenImport i -> Just i
      _ -> Nothing

bareIdentifier :: ParserT st e String
bareIdentifier = do
  h <- satisfy L.validFirstChar
  t <- many (satisfy L.validTailChar)
  return (h : t)

dottedIdentifier :: Parser e (NonEmpty String)
dottedIdentifier = nonEmpty' <$> sepBy1 bareIdentifier dot
  where
    dot :: Parser e ()
    dot = $(char '.')

pImport :: Parser e ImportScanParsed
pImport = do
  withSpan helper $ \_importNames _importLoc ->
    return ImportScan {..}
  where
    helper :: Parser e (NonEmpty String)
    helper = do
      lexeme $(string Str.import_)
      dottedIdentifier

pToken :: Parser e Token
pToken =
  lexeme $
    TokenString <$ pString
      <|> TokenImport <$> pImport
      <|> TokenReserved <$ pReserved
      <|> TokenCode <$ pCode

pCode :: Parser e ()
pCode = skipSome (satisfy validCodeChar)
  where
    validCodeChar :: Char -> Bool
    validCodeChar = (`notElem` reservedSymbols) .&&. (not . L.isWhiteSpace)

pReserved :: Parser e ()
pReserved = skipSatisfyAscii (`elem` reservedSymbols)

pString :: Parser e ()
pString = do
  $(char '"')
  body
  where
    body :: Parser e ()
    body = do
      c <- anyChar
      case c of
        '"' -> return ()
        '\\' -> skipAnyChar >> body
        _ -> body

comment :: forall e. Parser e ()
comment =
  lineComment
    <|> judocBlock
    <|> pragmaBlock
    <|> blockComment
  where
    nonNestedBlock :: Parser e () -> Parser e () -> Parser e ()
    nonNestedBlock start end = do
      let oneChar = skipAnyChar `notFollowedBy` end
      start
      skipMany oneChar
      end

    nestedBlock :: Parser e () -> Parser e () -> Parser e ()
    nestedBlock start end = do
      start
      go 1
      where
        go :: Int -> Parser e ()
        go n = do
          let startOrEnd =
                start $> True
                  <|> end $> False
          isStart <- snd <$> manyTill_ skipAnyChar startOrEnd
          if
              | isStart -> go (n + 1)
              | n > 1 -> go (n - 1)
              | otherwise -> return ()

    pragmaBlock :: Parser e ()
    pragmaBlock = nonNestedBlock $(string Str.pragmasStart) $(string Str.pragmasEnd)

    judocBlock :: Parser e ()
    judocBlock = nonNestedBlock $(string Str.judocBlockStart) $(string Str.judocBlockEnd)

    blockComment :: Parser e ()
    blockComment = nestedBlock $(string Str.commentBlockStart) $(string Str.commentBlockEnd)

    -- note that this also includes judoc line
    lineComment :: Parser e ()
    lineComment = do
      start
      void takeLine
      where
        start :: Parser e ()
        start = $(string Str.commentLineStart)
