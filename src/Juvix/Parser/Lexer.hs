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
import Text.Megaparsec as P hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char hiding (space, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type ParsecS r = ParsecT Void Text (Sem r)

parseFailure :: Int -> String -> ParsecS r a
parseFailure off str = P.parseError $ P.FancyError off (Set.singleton (P.ErrorFail str))

space1 :: (MonadParsec e s m, Token s ~ Char) => m ()
space1 = void $ takeWhile1P (Just "white space (only spaces and newlines allowed)") isWhiteSpace
  where
    isWhiteSpace :: Char -> Bool
    isWhiteSpace = (`elem` [' ', '\n'])

space' :: forall r. Bool -> ParsecS r [Comment]
space' special = do
  catMaybes
    <$> P.many
      ( hidden
          ( choice
              [space1 $> Nothing, Just <$> (lineComment <|> blockComment)]
          )
      )
  where
    lineComment :: ParsecS r Comment
    lineComment = do
      let _commentType = CommentOneLine
      when
        special
        (notFollowedBy (P.chunk Str.judocStart))
      (_commentText, _commentInterval) <- interval $ do
        void (P.chunk "--")
        P.takeWhileP Nothing (/= '\n')
      return Comment {..}

    blockComment :: ParsecS r Comment
    blockComment = do
      let _commentType = CommentBlock
      when
        special
        (notFollowedBy (P.chunk Str.pragmasStart))
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
delim' :: Text -> ParsecS r Interval
delim' d = P.label (unpack d) . fmap snd . interval $ chunk d

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

curLoc :: ParsecS r Loc
curLoc = do
  sp <- getSourcePos
  offset <- getOffset
  return (mkLoc offset sp)

onlyInterval :: ParsecS r a -> ParsecS r Interval
onlyInterval = fmap snd . interval

interval :: ParsecS r a -> ParsecS r (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

withLoc :: ParsecS r a -> ParsecS r (WithLoc a)
withLoc ma = do
  (a, i) <- interval ma
  return (WithLoc i a)
