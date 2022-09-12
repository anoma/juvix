-- | This module contains lexing functions common to all parsers in the pipeline
-- (Juvix, JuvixCore, JuvixAsm).
module Juvix.Parser.Lexer where

import Control.Monad.Trans.Class (lift)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Unicode
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Text.Megaparsec as P hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char hiding (space, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type ParsecS r = ParsecT Void Text (Sem r)

newtype ParserParams = ParserParams
  { _parserParamsRoot :: FilePath
  }

makeLenses ''ParserParams

parseFailure :: Int -> String -> ParsecS r a
parseFailure off str = P.parseError $ P.FancyError off (Set.singleton (P.ErrorFail str))

space1 :: (MonadParsec e s m, Token s ~ Char) => m ()
space1 = void $ takeWhile1P (Just "white space (only spaces and newlines allowed)") isWhiteSpace
  where
    isWhiteSpace :: Char -> Bool
    isWhiteSpace = (`elem` [' ', '\n'])

space' :: forall r. Bool -> (forall a. ParsecS r a -> ParsecS r ()) -> ParsecS r ()
space' judoc comment_ = L.space space1 lineComment block
  where
    lineComment :: ParsecS r ()
    lineComment = comment_ $ do
      when
        judoc
        (notFollowedBy (P.chunk Str.judocStart))
      void (P.chunk "--")
      void (P.takeWhileP Nothing (/= '\n'))

    block :: ParsecS r ()
    block = comment_ (L.skipBlockComment "{-" "-}")

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

keyword' :: ParsecS r () -> Text -> ParsecS r ()
keyword' spc kw = do
  P.try $ do
    P.chunk kw
    notFollowedBy (satisfy validTailChar)
    spc

keywordL' :: Member (Reader ParserParams) r => ParsecS r () -> Text -> ParsecS r Interval
keywordL' spc kw = do
  P.try $ do
    i <- snd <$> interval (P.chunk kw)
    notFollowedBy (satisfy validTailChar)
    spc
    return i

rawKeyword' :: ParsecS r () -> Text -> ParsecS r ()
rawKeyword' spc kw = do
  P.try $ do
    void (P.chunk kw)
    spc

rawIdentifier :: [ParsecS r ()] -> ParsecS r Text
rawIdentifier allKeywords = do
  notFollowedBy (choice allKeywords)
  h <- P.satisfy validFirstChar
  t <- P.takeWhileP Nothing validTailChar
  return (Text.cons h t)

validTailChar :: Char -> Bool
validTailChar c =
  isAlphaNum c || (validFirstChar c && notElem c delimiterSymbols)

delimiterSymbols :: [Char]
delimiterSymbols = ","

reservedSymbols :: [Char]
reservedSymbols = "\";(){}[].≔λ\\"

validFirstChar :: Char -> Bool
validFirstChar c = not (isNumber c || isSpace c || (c `elem` reservedSymbols))

curLoc :: Member (Reader ParserParams) r => ParsecS r Loc
curLoc = do
  sp <- getSourcePos
  offset <- getOffset
  root <- lift (asks (^. parserParamsRoot))
  return (mkLoc root offset sp)

interval :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (a, Interval)
interval ma = do
  start <- curLoc
  res <- ma
  end <- curLoc
  return (res, mkInterval start end)

withLoc :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (WithLoc a)
withLoc ma = do
  (a, i) <- interval ma
  return (WithLoc i a)
