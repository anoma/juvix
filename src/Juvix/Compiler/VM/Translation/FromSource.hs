{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Compiler.VM.Translation.FromSource
  ( module Juvix.Compiler.VM.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.VM.Extra.Utils
import Juvix.Compiler.VM.Language
import Juvix.Compiler.VM.Translation.FromSource.Lexer
import Juvix.Data.Keyword.All (kwHP, kwSP)
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: Text -> Either MegaparsecError [Instruction]
parseText = runParser ""

runParser :: FilePath -> Text -> Either MegaparsecError [Instruction]
runParser fileName input =
  case run $ P.runParserT parseToplevel fileName input of
    Left err -> Left (MegaparsecError err)
    Right instrs -> Right instrs

parseToplevel :: ParsecS r [Instruction]
parseToplevel = do
  instrs <- P.many statement
  P.eof
  return instrs

statement :: ParsecS r Instruction
statement = do
  space
  label <|> instruction

label :: ParsecS r Instruction
label = P.try $ do
  txt <- identifier
  kw kwColon
  return $ mkLabel txt

instruction :: ParsecS r Instruction
instruction = do
  off <- P.getOffset
  txt <- identifier
  case txt of
    "add" ->
      Binop <$> parseBinopArgs OpIntAdd
    "sub" ->
      Binop <$> parseBinopArgs OpIntSub
    "mul" ->
      Binop <$> parseBinopArgs OpIntMul
    "div" ->
      Binop <$> parseBinopArgs OpIntDiv
    "mod" ->
      Binop <$> parseBinopArgs OpIntMod
    "lt" ->
      Binop <$> parseBinopArgs OpIntLt
    "eq" ->
      Binop <$> parseBinopArgs OpIntEq
    "jumpz" ->
      JumpOnZero <$> parseJumpOnZeroArgs
    "halt" ->
      return Halt
    "move" ->
      Move <$> parseMoveArgs
    "jump" ->
      Jump <$> parseJumpArgs
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

registerSP :: ParsecS r Int
registerSP = do
  kw kwSP
  return 0

registerHP :: ParsecS r Int
registerHP = do
  kw kwHP
  return 1

register :: ParsecS r Int
register = registerSP <|> registerHP <|> registerR

indirect :: ParsecS r Int
indirect = do
  symbol "["
  r <- register
  symbol "]"
  return r

lvalue :: ParsecS r LValue
lvalue = (LMemRef <$> indirect) <|> (LRegRef <$> register)

parseBinopArgs :: Opcode -> ParsecS r BinaryOp
parseBinopArgs op = do
  lval <- lvalue
  comma
  val1 <- value
  comma
  val2 <- value
  return $ BinaryOp op lval val1 val2

parseMoveArgs :: ParsecS r InstrMove
parseMoveArgs = do
  lval <- lvalue
  comma
  val <- value
  return $ InstrMove lval val

parseJumpArgs :: ParsecS r InstrJump
parseJumpArgs = InstrJump <$> value

parseJumpOnZeroArgs :: ParsecS r InstrJumpOnZero
parseJumpOnZeroArgs = do
  val <- value
  comma
  dest <- value
  return $ InstrJumpOnZero val dest

value :: ParsecS r Value
value = indirectRef <|> registerRef <|> integerValue <|> varValue <|> labelValue

indirectRef :: ParsecS r Value
indirectRef = MemRef <$> indirect

registerRef :: ParsecS r Value
registerRef = RegRef <$> register

integerValue :: ParsecS r Value
integerValue = Const <$> smallInt

varValue :: ParsecS r Value
varValue = do
  symbol "$"
  VarRef <$> identifier

labelValue :: ParsecS r Value
labelValue = LabelRef <$> identifier
