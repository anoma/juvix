-- | Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix/Parser*
module MiniJuvix.Utils.Parser
  ( Parser,
    ParserError,

    -- * Tokens
    charToWord8,
    toChar,
    validUpperSymbol,
    validStartSymbol',
    dash,
    percent,
    slash,
    newLine,
    backtick,
    hat,
    asterisk,
    amper,
    bang,
    question,
    dot,
    at,
    equals,
    pipe,
    doubleQuote,
    quote,
    backSlash,
    closeBracket,
    openBracket,
    closeCurly,
    openCurly,
    closeParen,
    openParen,
    hash,
    comma,
    semi,
    colon,
    space,
    under,
    validStartSymbol,
    validMiddleSymbol,
    validInfixSymbol,
    endOfLine,
    reservedWords,
    reservedSymbols,

    -- * Lexing
    emptyCheck,
    spacer,
    spaceLiner,
    skipLiner,
    eatSpaces,
    between,
    parens,
    brackets,
    curly,
    many1H,
    sepBy1HFinal,
    sepBy1,
    sepBy1H,
    maybeParend,
    integer,
  )
where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Word8 as Word8
import qualified GHC.Unicode as Unicode
import MiniJuvix.Utils.Prelude
import qualified MiniJuvix.Utils.Prelude as Set
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P

--------------------------------------------------------------------------------

type Parser = P.Parsec Void ByteString

--                   ^    ^
--                   |    |
-- Custom error component Type of input stream

type ParserError = P.ParseErrorBundle ByteString Void

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

toChar :: Integral a => a -> Char
toChar = chr . fromIntegral

-- Hopefully this is fast!
validStartSymbol' :: Integral a => a -> Bool
validStartSymbol' = Unicode.isAlpha . toChar

-- Unicode.isUpper 'İ' = True!
validUpperSymbol :: Integral a => a -> Bool
validUpperSymbol = Unicode.isUpper . toChar

dash :: Word8
dash = charToWord8 '-'

under :: Word8
under = charToWord8 '_'

space :: Word8
space = charToWord8 ' '

colon :: Word8
colon = charToWord8 ':'

semi :: Word8
semi = charToWord8 ';'

comma :: Word8
comma = charToWord8 ','

hash :: Word8
hash = charToWord8 '#'

openParen :: Word8
openParen = charToWord8 '('

closeParen :: Word8
closeParen = charToWord8 ')'

openCurly :: Word8
openCurly = charToWord8 '{'

closeCurly :: Word8
closeCurly = charToWord8 '}'

openBracket :: Word8
openBracket = charToWord8 '['

closeBracket :: Word8
closeBracket = charToWord8 ']'

backSlash :: Word8
backSlash = charToWord8 '\\'

quote :: Word8
quote = charToWord8 '\''

doubleQuote :: Word8
doubleQuote = charToWord8 '\"'

pipe :: Word8
pipe = charToWord8 '|'

equals :: Word8
equals = charToWord8 '='

at :: Word8
at = charToWord8 '@'

dot :: Word8
dot = charToWord8 '.'

question :: Word8
question = charToWord8 '?'

bang :: Word8
bang = charToWord8 '!'

amper :: Word8
amper = charToWord8 '&'

asterisk :: Word8
asterisk = charToWord8 '*'

hat :: Word8
hat = charToWord8 '^'

backtick :: Word8
backtick = charToWord8 '`'

newLine :: Word8
newLine = charToWord8 '\n'

slash :: Word8
slash = charToWord8 '/'

percent :: Word8
percent = charToWord8 '%'

validStartSymbol :: Word8 -> Bool
validStartSymbol w =
  validStartSymbol' w || w == under

validInfixSymbol :: Word8 -> Bool
validInfixSymbol w =
  Unicode.isSymbol (toChar w)
    || w == asterisk
    || w == hat
    || w == dash
    || w == amper
    || w == colon
    || w == slash
    || w == percent
    || w == dot

validMiddleSymbol :: Word8 -> Bool
validMiddleSymbol w =
  validStartSymbol w
    || Word8.isDigit w
    || w == dash
    || w == bang
    || w == question
    || w == percent

-- check for \r or \n
endOfLine :: (Eq a, Num a) => a -> Bool
endOfLine w = w == 13 || w == 10

-- Reserved words and symbols.

reservedWords :: (Ord a, IsString a) => Set a
reservedWords =
  Set.fromList
    [ "let",
      "in",
      "case",
      "open",
      "import",
      "if",
      "then",
      "end",
      "begin",
      "module",
      "where"
    ]

reservedSymbols :: (Ord a, IsString a) => Set a
reservedSymbols = Set.fromList [":", "=", "|", "", "--"]

--------------------------------------------------------------------------------
-- Lexing
--------------------------------------------------------------------------------

emptyCheck :: Word8 -> Bool
emptyCheck x = Word8.isSpace x || x == newLine

spacer :: Parser p -> Parser p
spacer p = P.takeWhileP (Just "spacer") Word8.isSpace *> p

spaceLiner :: Parser p -> Parser p
spaceLiner p = do
  p <* P.takeWhileP (Just "space liner") emptyCheck

skipLiner :: Word8 -> Parser ()
skipLiner p = spaceLiner (P.skipCount 1 (P.char p))

eatSpaces :: Parser p -> Parser p
eatSpaces p = P.takeWhileP (Just "eat spaces") emptyCheck *> p

between :: Word8 -> Parser p -> Word8 -> Parser p
between x p end = skipLiner x *> spaceLiner p <* P.satisfy (== end)

parens :: Parser p -> Parser p
parens p = between openParen p closeParen

brackets :: Parser p -> Parser p
brackets p = between openBracket p closeBracket

curly :: Parser p -> Parser p
curly p = between openCurly p closeCurly

many1H :: Parser a -> Parser (NonEmpty a)
many1H = fmap fromList . P.some

-- | 'sepBy1HFinal' is like 'sepBy1H' but also tries to parse a last separator
sepBy1HFinal :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1HFinal parse sep = sepBy1H parse sep <* P.optional sep

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (P.try $ sep *> p))

sepBy1H :: Parser a -> Parser s -> Parser (NonEmpty a)
sepBy1H parse sep = fromList <$> sepBy1 parse sep

maybeParend :: Parser a -> Parser a
maybeParend p = p <|> parens p

integer :: Parser Integer
integer = do
  digits <- P.takeWhileP (Just "digits") Word8.isDigit
  case Char8.readInteger digits of
    Just (x, _) -> pure x
    Nothing -> fail "didn't parse an int"
