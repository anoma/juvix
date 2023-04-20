-- | This module contains lexing functions common to all parsers in the pipeline
-- (Juvix, JuvixCore, JuvixAsm).
module Juvix.Parser.Lexer where

import Data.HashSet qualified as HashSet
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Unicode
import Juvix.Data.Keyword
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Parsing as P hiding (hspace, space, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type ParsecS r = ParsecT Void Text (Sem r)

parseFailure :: Int -> String -> ParsecS r a
parseFailure off str = P.parseError $ P.FancyError off (Set.singleton (P.ErrorFail str))

whiteSpace1 :: (MonadParsec e s m, Token s ~ Char) => m ()
whiteSpace1 = void (takeWhile1P (Just spaceMsg) isWhiteSpace)

whiteSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
whiteSpace = void (takeWhileP (Just spaceMsg) isWhiteSpace)

isWhiteSpace :: Char -> Bool
isWhiteSpace = (`elem` [' ', '\n'])

hspace :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
hspace = takeWhileP (Just spaceMsg) isHWhiteSpace
  where
    isHWhiteSpace :: Char -> Bool
    isHWhiteSpace = (== ' ')

hspace_ :: (MonadParsec e s m, Token s ~ Char) => m ()
hspace_ = void hspace

spaceMsg :: String
spaceMsg = "white space (only spaces and newlines allowed)"

space' :: forall e m. MonadParsec e Text m => Bool -> m [Comment]
space' judoc = comments
  where
    comments :: m [Comment]
    comments = hidden (go [])

    go :: [Comment] -> m [Comment]
    go acc = do
      s <- sepSpace
      m <- optional (comment s)
      case m of
        Nothing -> return (reverse acc)
        Just c -> go (c : acc)
      where
        -- Returns `True` if it consumes at least one empty line
        sepSpace :: m Bool
        sepSpace = do
          hspace_
          s <- isJust <$> optional (P.try (newline >> hspace_ >> newline))
          optional whiteSpace
          return s

    comment :: Bool -> m Comment
    comment _commentPreceedingEmptyLine = lineComment <|> blockComment
      where
        lineComment :: m Comment
        lineComment = do
          let _commentType = CommentOneLine
          when
            judoc
            (notFollowedBy (P.chunk Str.judocStart))
          (_commentText, _commentInterval) <- interval $ do
            void (P.chunk "--")
            P.takeWhileP Nothing (/= '\n')
          return Comment {..}

        blockComment :: m Comment
        blockComment = do
          let _commentType = CommentBlock
          (_commentText, _commentInterval) <- interval $ do
            void (P.chunk "{-")
            pack <$> P.manyTill anySingle (P.chunk "-}")
          return Comment {..}

integer' :: ParsecS r (Integer, Interval) -> ParsecS r (Integer, Interval)
integer' dec = do
  minus <- optional (char '-')
  (nat, i) <- dec
  let nat' = case minus of
        Nothing -> nat
        _ -> (-nat)
  return (nat', i)

number' :: ParsecS r (Integer, Interval) -> Int -> Int -> ParsecS r (Int, Interval)
number' int mn mx = do
  off <- getOffset
  (n, i) <- int
  when
    (n < fromIntegral mn || n > fromIntegral mx)
    (parseFailure off ("number out of bounds: " ++ show n))
  return (fromInteger n, i)

string' :: ParsecS r Text
string' = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

-- | The caller is responsible of consuming space after it.
kw' :: Keyword -> ParsecS r KeywordRef
kw' k@Keyword {..} = P.label (unpack _keywordAscii) (reserved <|> normal)
  where
    -- If the ascii representation uses reserved symbols, we use chunk so that we parse exactly the keyword
    -- (if chunk fails it does not consume anything so try is not needed)
    reserved :: ParsecS r KeywordRef
    reserved
      | _keywordHasReserved = do
          i <- onlyInterval (P.chunk _keywordAscii)
          return (KeywordRef k i Ascii)
      | otherwise = empty
    -- we parse the longest valid identifier and then we check if it is the expected keyword
    normal :: ParsecS r KeywordRef
    normal = P.try $ do
      (w, i) <- interval morpheme
      case keywordMatch k w of
        Just u -> return (KeywordRef k i u)
        Nothing -> failure Nothing (Set.singleton (Label (fromJust $ nonEmpty $ unpack _keywordAscii)))

rawIdentifier' :: (Char -> Bool) -> HashSet Text -> ParsecS r Text
rawIdentifier' excludedTailChar allKeywords = label "<identifier>" $ P.try $ do
  w <- morpheme' excludedTailChar
  when (w `HashSet.member` allKeywords) empty
  return w

rawIdentifier :: HashSet Text -> ParsecS r Text
rawIdentifier = rawIdentifier' (const False)

validTailChar :: Char -> Bool
validTailChar =
  (`notElem` reservedSymbols)
    .&&. (isAlphaNum .||. (validFirstChar .&&. (`notElem` delimiterSymbols)))

-- | A word that does not contain reserved symbols. It may be an identifier or a keyword.
morpheme' :: (Char -> Bool) -> ParsecS r Text
morpheme' excludedTailChar = do
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP Nothing (validTailChar .&&. not . excludedTailChar)
  let iden = Text.cons h t
  return iden

morpheme :: ParsecS r Text
morpheme = morpheme' (const False)

delimiterSymbols :: [Char]
delimiterSymbols = ","

isDelimiterStr :: Text -> Bool
isDelimiterStr t = case unpack t of
  [c] -> isDelimiter c
  _ -> False

isDelimiter :: Char -> Bool
isDelimiter = (`elem` delimiterSymbols)

validFirstChar :: Char -> Bool
validFirstChar c = not (isNumber c || isSpace c || (c `elem` reservedSymbols))

curLoc :: MonadParsec e Text m => m Loc
curLoc = do
  sp <- getSourcePos
  offset <- getOffset
  return (mkLoc offset sp)

onlyInterval :: ParsecS r a -> ParsecS r Interval
onlyInterval = fmap snd . interval

interval :: MonadParsec e Text m => m a -> m (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

withLoc :: ParsecS r a -> ParsecS r (WithLoc a)
withLoc ma = do
  (a, i) <- interval ma
  return (WithLoc i a)
