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

string :: ParsecS r Text
string = string'

boolean :: Member (Reader ParserParams) r => ParsecS r (Bool, Interval)
boolean = interval (kwTrue >> return True) <|> interval (kwFalse >> return False)

keyword :: Text -> ParsecS r ()
keyword = void . keyword' space

identifier :: ParsecS r Text
identifier = lexeme bareIdentifier

identifierL :: Member (Reader ParserParams) r => ParsecS r (Text, Interval)
identifierL = lexemeInterval bareIdentifier

-- | Same as @identifier@ but does not consume space after it.
bareIdentifier :: ParsecS r Text
bareIdentifier = rawIdentifier allKeywords

allKeywords :: [ParsecS r ()]
allKeywords =
  [ kwAssignment,
    kwColon,
    kwLambda,
    kwLet,
    kwIn,
    kwConstr,
    kwCase,
    kwOf,
    kwIf,
    kwThen,
    kwElse,
    kwTrue,
    kwFalse,
    kwDef,
    kwMapsTo,
    kwRightArrow,
    kwSemicolon,
    kwWildcard,
    kwPlus,
    kwMinus,
    kwMul,
    kwDiv,
    kwEq,
    kwLt,
    kwLe,
    kwGt,
    kwGe
  ]

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

kwAssignment :: ParsecS r ()
kwAssignment = keyword Str.assignUnicode <|> keyword Str.assignAscii

kwColon :: ParsecS r ()
kwColon = keyword Str.colon

kwInductive :: ParsecS r ()
kwInductive = keyword Str.inductive

kwLambda :: ParsecS r ()
kwLambda = keyword Str.lambdaUnicode <|> keyword Str.lambdaAscii

kwLet :: ParsecS r ()
kwLet = keyword Str.let_

kwIn :: ParsecS r ()
kwIn = keyword Str.in_

kwConstr :: ParsecS r ()
kwConstr = keyword Str.constr

kwCase :: ParsecS r ()
kwCase = keyword Str.case_

kwOf :: ParsecS r ()
kwOf = keyword Str.of_

kwIf :: ParsecS r ()
kwIf = keyword Str.if_

kwThen :: ParsecS r ()
kwThen = keyword Str.then_

kwElse :: ParsecS r ()
kwElse = keyword Str.else_

kwTrue :: ParsecS r ()
kwTrue = keyword Str.true_

kwFalse :: ParsecS r ()
kwFalse = keyword Str.false_

kwDef :: ParsecS r ()
kwDef = keyword Str.def

kwMapsTo :: ParsecS r ()
kwMapsTo = keyword Str.mapstoUnicode <|> keyword Str.mapstoAscii

kwRightArrow :: ParsecS r ()
kwRightArrow = keyword Str.toUnicode <|> keyword Str.toAscii

kwSemicolon :: ParsecS r ()
kwSemicolon = keyword Str.semicolon

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
