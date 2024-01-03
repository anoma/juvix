module Juvix.Compiler.Casm.Translation.FromSource where

import Control.Monad.Trans.Class
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromSource.Lexer
import Juvix.Data.Keyword.All (kwAP, kwAbs, kwCall, kwEq, kwFP, kwIf, kwJmp, kwMinus, kwMul, kwNotEq, kwPlus, kwPlusEq, kwRel, kwRet)
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: Text -> Either MegaparsecError (LabelInfo, [Instruction])
parseText = runParser ""

runParser :: FilePath -> Text -> Either MegaparsecError (LabelInfo, [Instruction])
runParser fileName input =
  case run $ runLabelInfoBuilder $ P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (MegaparsecError err)
    (li, Right instrs) -> Right (li, instrs)

parseToplevel :: (Member LabelInfoBuilder r) => ParsecS r [Instruction]
parseToplevel = do
  instrs <- statements 0
  P.eof
  return instrs

statements :: (Member LabelInfoBuilder r) => Offset -> ParsecS r [Instruction]
statements off = do
  space
  label' off <|> statement' off <|> return []

statement' :: (Member LabelInfoBuilder r) => Offset -> ParsecS r [Instruction]
statement' off = do
  i <- instruction
  (i :) <$> statements (off + 1)

label' :: (Member LabelInfoBuilder r) => Offset -> ParsecS r [Instruction]
label' off = label off >> statements (off + 1)

label :: (Member LabelInfoBuilder r) => Offset -> ParsecS r ()
label off = P.try $ do
  off' <- P.getOffset
  txt <- identifier
  kw kwColon
  msym <- lift $ getIdent txt
  case msym of
    Nothing -> do
      sym <- lift freshSymbol
      lift $ registerLabelName sym txt
      lift $ registerLabelOffset sym off
    Just {} -> do
      parseFailure off' "duplicate label"

instruction :: (Member LabelInfoBuilder r) => ParsecS r Instruction
instruction =
  parseAlloc <|> parseJump <|> parseCall <|> parseReturn <|> parseAssign

parseAlloc :: ParsecS r Instruction
parseAlloc = do
  kw kwAP
  kw kwPlusEq
  i <- int
  return $
    Alloc $
      InstrAlloc
        { _instrAllocSize = i
        }

parseIsRel :: ParsecS r Bool
parseIsRel = (kw kwRel >> return True) <|> (kw kwAbs >> return False)

parseValue :: (Member LabelInfoBuilder r) => ParsecS r Value
parseValue = (Imm <$> parseImm) <|> (Ref <$> parseMemRef) <|> (Label <$> parseLabel)

parseImm :: ParsecS r Immediate
parseImm = fst <$> integer

parseMemRef :: ParsecS r MemRef
parseMemRef = do
  lbracket
  r <- register
  kw kwPlus
  off <- offset
  rbracket
  return $ MemRef {_memRefReg = r, _memRefOff = off}

parseLabel :: (Member LabelInfoBuilder r) => ParsecS r LabelRef
parseLabel = do
  txt <- identifier
  msym <- lift $ getIdent txt
  case msym of
    Nothing -> do
      sym <- lift freshSymbol
      lift $ registerLabelName sym txt
      return $ LabelRef {_labelRefName = Just txt, _labelRefSymbol = sym}
    Just sym ->
      return $ LabelRef {_labelRefName = Just txt, _labelRefSymbol = sym}

parseJump :: forall r. (Member LabelInfoBuilder r) => ParsecS r Instruction
parseJump = do
  kw kwJmp
  isRel <- parseIsRel
  tgt <- parseValue
  mv <- optional if_
  case mv of
    Nothing ->
      return $ Jump $ InstrJump {_instrJumpTarget = tgt, _instrJumpIsRel = isRel}
    Just v ->
      return $
        JumpIf $
          InstrJumpIf
            { _instrJumpIfTarget = tgt,
              _instrJumpIfIsRel = isRel,
              _instrJumpIfValue = v
            }
  where
    if_ :: ParsecS r Value
    if_ = do
      kw kwIf
      v <- parseValue
      kw kwNotEq
      symbol "0"
      return v

parseCall :: (Member LabelInfoBuilder r) => ParsecS r Instruction
parseCall = do
  kw kwCall
  isRel <- parseIsRel
  v <- parseValue
  return $ Call $ InstrCall {_instrCallTarget = v, _instrCallIsRel = isRel}

parseReturn :: ParsecS r Instruction
parseReturn = do
  kw kwRet
  return Return

parseAssign :: forall r. (Member LabelInfoBuilder r) => ParsecS r Instruction
parseAssign = do
  res <- parseMemRef
  kw kwEq
  load res <|> binop res <|> asn res
  where
    load :: MemRef -> ParsecS r Instruction
    load res = P.try $ do
      lbracket
      src <- parseMemRef
      kw kwPlus
      off <- offset
      rbracket
      return $
        Load $
          InstrLoad
            { _instrLoadResult = res,
              _instrLoadSrc = src,
              _instrLoadOff = off
            }

    binop :: MemRef -> ParsecS r Instruction
    binop res = P.try $ do
      arg1 <- parseMemRef
      op <- opcode
      arg2 <- parseValue
      return $
        Binop $
          InstrBinop
            { _instrBinopOpcode = op,
              _instrBinopResult = res,
              _instrBinopArg1 = arg1,
              _instrBinopArg2 = arg2
            }

    asn :: MemRef -> ParsecS r Instruction
    asn res = do
      v <- parseValue
      return $ Assign $ InstrAssign {_instrAssignValue = v, _instrAssignResult = res}

registerAP :: ParsecS r Reg
registerAP = do
  kw kwAP
  return Ap

registerFP :: ParsecS r Reg
registerFP = do
  kw kwFP
  return Fp

register :: ParsecS r Reg
register = registerAP <|> registerFP

opcode :: ParsecS r Opcode
opcode =
  (kw kwPlus >> return FieldAdd)
    <|> (kw kwMinus >> return FieldSub)
    <|> (kw kwMul >> return FieldMul)
