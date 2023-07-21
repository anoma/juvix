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
    "load" ->
      Load <$> parseLoadArgs
    "store" ->
      Store <$> parseStoreArgs
    "move" ->
      Move <$> parseMoveArgs
    "halt" ->
      return Halt
    "jump" ->
      Jump <$> parseJumpArgs
    "jumpz" ->
      JumpOnZero <$> parseJumpOnZeroArgs
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

parseBinopArgs :: Opcode -> ParsecS r BinaryOp
parseBinopArgs op = do
  reg <- register
  comma
  val1 <- value
  comma
  val2 <- value
  return $ BinaryOp op reg val1 val2

parseLoadArgs :: ParsecS r InstrLoad
parseLoadArgs = do
  dest <- register
  comma
  src <- register
  comma
  off <- offset
  return $ InstrLoad dest src off

parseStoreArgs :: ParsecS r InstrStore
parseStoreArgs = do
  dest <- register
  comma
  off <- offset
  comma
  val <- value
  return $ InstrStore dest off val

parseMoveArgs :: ParsecS r InstrMove
parseMoveArgs = do
  dest <- register
  comma
  val <- value
  return $ InstrMove dest val

parseAllocArgs :: ParsecS r InstrAlloc
parseAllocArgs = do
  dest <- register
  comma
  val <- value
  return $ InstrAlloc dest val

parsePushArgs :: ParsecS r InstrPush
parsePushArgs = InstrPush <$> value

parsePopArgs :: ParsecS r InstrPop
parsePopArgs = InstrPop <$> register

parseJumpArgs :: ParsecS r InstrJump
parseJumpArgs = InstrJump <$> value

parseJumpOnZeroArgs :: ParsecS r InstrJumpOnZero
parseJumpOnZeroArgs = do
  reg <- register
  comma
  val <- value
  return $ InstrJumpOnZero reg val

value :: ParsecS r Value
value = registerRef <|> integerValue <|> varValue <|> labelValue

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
