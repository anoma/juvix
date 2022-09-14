module Juvix.Compiler.Core.Translation.FromSource.Lexer
  ( module Juvix.Compiler.Core.Translation.FromSource.Lexer,
    module Juvix.Parser.Lexer,
  )
where

import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Lexer
import Juvix.Prelude
import Text.Megaparsec as P hiding (sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char.Lexer qualified as L

space :: ParsecS r ()
space = space' False void

lexeme :: ParsecS r a -> ParsecS r a
lexeme = L.lexeme space

lexemeInterval :: Member (Reader ParserParams) r => ParsecS r a -> ParsecS r (a, Interval)
lexemeInterval = lexeme . interval

symbol :: Text -> ParsecS r ()
symbol = void . L.symbol space

decimal :: (Member (Reader ParserParams) r, Num n) => ParsecS r (n, Interval)
decimal = lexemeInterval L.decimal

integer :: Member (Reader ParserParams) r => ParsecS r (Integer, Interval)
integer = integer' decimal

number :: Member (Reader ParserParams) r => Int -> Int -> ParsecS r (Int, Interval)
number = number' integer

string :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
string = lexemeInterval string'

keyword :: Text -> ParsecS r ()
keyword = keyword' space

keywordSymbol :: Text -> ParsecS r ()
keywordSymbol = keywordSymbol' space

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier allKeywords

allKeywords :: [ParsecS r ()]
allKeywords =
  [ kwAssign,
    kwLet,
    kwLetRec,
    kwIn,
    kwConstr,
    kwCase,
    kwOf,
    kwMatch,
    kwWith,
    kwIf,
    kwThen,
    kwElse,
    kwDef,
    kwRightArrow,
    kwSemicolon,
    kwComma,
    kwWildcard,
    kwPlus,
    kwMinus,
    kwMul,
    kwDiv,
    kwMod,
    kwEq,
    kwLt,
    kwLe,
    kwGt,
    kwGe,
    kwBind,
    kwSeq,
    kwTrace,
    kwFail
  ]

symbolAt :: ParsecS r ()
symbolAt = symbol Str.at_

lambda :: ParsecS r ()
lambda = symbol Str.lambdaUnicode <|> symbol Str.lambdaAscii

lbrace :: ParsecS r ()
lbrace = symbol "{"

rbrace :: ParsecS r ()
rbrace = symbol "}"

lparen :: ParsecS r ()
lparen = symbol "("

rparen :: ParsecS r ()
rparen = symbol ")"

parens :: ParsecS r a -> ParsecS r a
parens = between lparen rparen

braces :: ParsecS r a -> ParsecS r a
braces = between (symbol "{") (symbol "}")

kwAssign :: ParsecS r ()
kwAssign = keyword Str.assignUnicode <|> keyword Str.assignAscii

kwLet :: ParsecS r ()
kwLet = keyword Str.let_

kwLetRec :: ParsecS r ()
kwLetRec = keyword Str.letrec_

kwIn :: ParsecS r ()
kwIn = keyword Str.in_

kwConstr :: ParsecS r ()
kwConstr = keyword Str.constr

kwCase :: ParsecS r ()
kwCase = keyword Str.case_

kwOf :: ParsecS r ()
kwOf = keyword Str.of_

kwMatch :: ParsecS r ()
kwMatch = keyword Str.match_

kwWith :: ParsecS r ()
kwWith = keyword Str.with_

kwIf :: ParsecS r ()
kwIf = keyword Str.if_

kwThen :: ParsecS r ()
kwThen = keyword Str.then_

kwElse :: ParsecS r ()
kwElse = keyword Str.else_

kwDef :: ParsecS r ()
kwDef = keyword Str.def

kwRightArrow :: ParsecS r ()
kwRightArrow = keyword Str.toUnicode <|> keyword Str.toAscii

kwSemicolon :: ParsecS r ()
kwSemicolon = keyword Str.semicolon

kwComma :: ParsecS r ()
kwComma = keywordSymbol Str.comma

kwWildcard :: ParsecS r ()
kwWildcard = keyword Str.underscore

kwPlus :: ParsecS r ()
kwPlus = keyword Str.plus

kwMinus :: ParsecS r ()
kwMinus = keyword Str.minus

kwMul :: ParsecS r ()
kwMul = keyword Str.mul

kwDiv :: ParsecS r ()
kwDiv = keyword Str.div

kwMod :: ParsecS r ()
kwMod = keyword Str.mod

kwEq :: ParsecS r ()
kwEq = keyword Str.equal

kwLt :: ParsecS r ()
kwLt = keyword Str.less

kwLe :: ParsecS r ()
kwLe = keyword Str.lessEqual

kwGt :: ParsecS r ()
kwGt = keyword Str.greater

kwGe :: ParsecS r ()
kwGe = keyword Str.greaterEqual

kwBind :: ParsecS r ()
kwBind = keyword Str.bind

kwSeq :: ParsecS r ()
kwSeq = keyword Str.seq_

kwTrace :: ParsecS r ()
kwTrace = keyword Str.trace_

kwFail :: ParsecS r ()
kwFail = keyword Str.fail_
