module Juvix.Compiler.Casm.Translation.FromSource where

import Control.Monad.Trans.Class
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromSource.Lexer
import Juvix.Extra.Paths
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: Text -> Either MegaparsecError (LabelInfo, [Instruction])
parseText = runParser noFile

runParser :: Path Abs File -> Text -> Either MegaparsecError (LabelInfo, [Instruction])
runParser fileName input_ =
  case run . runLabelInfoBuilder $ runParser' 0 (toFilePath fileName) input_ of
    (_, Left err) -> Left err
    (li, Right instrs) -> Right (li, instrs)

runParser' :: (Member LabelInfoBuilder r) => Address -> FilePath -> Text -> Sem r (Either MegaparsecError [Instruction])
runParser' addr fileName input_ = do
  e <- P.runParserT (parseToplevel addr) fileName input_
  return $ case e of
    Left err -> Left (MegaparsecError err)
    Right instrs -> Right instrs

parseToplevel :: (Member LabelInfoBuilder r) => Address -> ParsecS r [Instruction]
parseToplevel addr = do
  instrs <- statements addr
  P.eof
  return instrs

statements :: (Member LabelInfoBuilder r) => Address -> ParsecS r [Instruction]
statements addr = do
  space
  label' addr <|> statement' addr <|> return []

statement' :: (Member LabelInfoBuilder r) => Address -> ParsecS r [Instruction]
statement' addr = do
  i <- instruction
  (i :) <$> statements (addr + 1)

label' :: (Member LabelInfoBuilder r) => Address -> ParsecS r [Instruction]
label' addr = do
  l <- label addr
  (l :) <$> statements (addr + 1)

label :: (Member LabelInfoBuilder r) => Address -> ParsecS r Instruction
label addr = P.try $ do
  off' <- P.getOffset
  txt <- identifier
  kw kwColon
  msym <- lift $ getIdent txt
  case msym of
    Nothing -> do
      sym <- lift freshSymbol
      lift $ registerLabelName sym txt
      lift $ registerLabelAddress sym addr
      return $ Label $ LabelRef {_labelRefSymbol = sym, _labelRefName = Just txt}
    Just sym -> do
      b <- lift $ hasOffset sym
      if
          | b -> parseFailure off' "duplicate label"
          | otherwise -> do
              lift $ registerLabelAddress sym addr
              return $ Label $ LabelRef {_labelRefSymbol = sym, _labelRefName = Just txt}

instruction :: (Member LabelInfoBuilder r) => ParsecS r Instruction
instruction =
  parseNop <|> parseAlloc <|> parseJump <|> parseCall <|> parseReturn <|> parseTrace <|> parseAssign

parseNop :: ParsecS r Instruction
parseNop = do
  kw kwNop
  return Nop

parseAlloc :: (Member LabelInfoBuilder r) => ParsecS r Instruction
parseAlloc = do
  kw kwAp
  kw kwPlusEq
  i <- parseRValue
  return $
    Alloc $
      InstrAlloc
        { _instrAllocSize = i
        }

parseRValue :: forall r. (Member LabelInfoBuilder r) => ParsecS r RValue
parseRValue = load <|> binop <|> val
  where
    load :: ParsecS r RValue
    load = P.try $ do
      lbracket
      src <- parseMemRef
      off <- parseOffset
      rbracket
      return $
        Load $
          LoadValue
            { _loadValueSrc = src,
              _loadValueOff = off
            }

    binop :: ParsecS r RValue
    binop = P.try $ do
      arg1 <- parseMemRef
      subconst arg1 <|> oper arg1
      where
        subconst :: MemRef -> ParsecS r RValue
        subconst arg1 = do
          kw kwMinus
          v <- parseImm
          return $
            Binop $
              BinopValue
                { _binopValueOpcode = FieldAdd,
                  _binopValueArg1 = arg1,
                  _binopValueArg2 = Imm (-v)
                }

        oper :: MemRef -> ParsecS r RValue
        oper arg1 = do
          op <- opcode
          arg2 <- parseValue
          return $
            Binop $
              BinopValue
                { _binopValueOpcode = op,
                  _binopValueArg1 = arg1,
                  _binopValueArg2 = arg2
                }

    val :: ParsecS r RValue
    val = Val <$> parseValue

parseValue :: (Member LabelInfoBuilder r) => ParsecS r Value
parseValue = (Imm <$> parseImm) <|> (Ref <$> parseMemRef) <|> (Lab <$> parseLabel)

parseImm :: ParsecS r Immediate
parseImm = fst <$> integer

parseOffset :: ParsecS r Offset
parseOffset =
  (kw kwPlus >> offset)
    <|> (kw kwMinus >> (negate <$> offset))
    <|> return 0

parseMemRef :: ParsecS r MemRef
parseMemRef = do
  lbracket
  r <- register
  off <- parseOffset
  rbracket
  return MemRef {_memRefReg = r, _memRefOff = off}

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

parseIncAp :: ParsecS r Bool
parseIncAp = (kw delimSemicolon >> kw kwApPlusPlus >> return True) <|> return False

parseRel :: ParsecS r Bool
parseRel = (kw kwRel >> return True) <|> (kw kwAbs >> return False) <|> return True

parseJump :: forall r. (Member LabelInfoBuilder r) => ParsecS r Instruction
parseJump = do
  kw kwJmp
  P.try jmpIf <|> jmp
  where
    jmpIf :: ParsecS r Instruction
    jmpIf = do
      tgt <- parseValue
      kw kwIf
      v <- parseMemRef
      kw kwNotEq
      symbol "0"
      incAp <- parseIncAp
      return $
        JumpIf $
          InstrJumpIf
            { _instrJumpIfTarget = tgt,
              _instrJumpIfValue = v,
              _instrJumpIfIncAp = incAp
            }

    jmp :: ParsecS r Instruction
    jmp = do
      isRel <- parseRel
      tgt <- parseRValue
      incAp <- parseIncAp
      return $
        Jump $
          InstrJump
            { _instrJumpTarget = tgt,
              _instrJumpRel = isRel,
              _instrJumpIncAp = incAp
            }

parseCall :: (Member LabelInfoBuilder r) => ParsecS r Instruction
parseCall = do
  kw kwCall
  isRel <- parseRel
  v <- parseValue
  return $ Call $ InstrCall {_instrCallTarget = v, _instrCallRel = isRel}

parseReturn :: ParsecS r Instruction
parseReturn = do
  kw kwRet
  return Return

parseTrace :: (Member LabelInfoBuilder r) => ParsecS r Instruction
parseTrace = do
  kw kwTrace
  v <- parseRValue
  return $ Trace $ InstrTrace {_instrTraceValue = v}

parseAssign :: forall r. (Member LabelInfoBuilder r) => ParsecS r Instruction
parseAssign = do
  res <- parseMemRef
  kw kwEq
  extraBinop res <|> asn res
  where
    asn :: MemRef -> ParsecS r Instruction
    asn res = do
      v <- parseRValue
      incAp <- parseIncAp
      return $
        Assign $
          InstrAssign
            { _instrAssignValue = v,
              _instrAssignResult = res,
              _instrAssignIncAp = incAp
            }

    extraBinop :: MemRef -> ParsecS r Instruction
    extraBinop res = P.try $ do
      arg1 <- parseMemRef
      op <- extraOpcode
      arg2 <- parseExtraValue op
      incAp <- parseIncAp
      return $
        ExtraBinop $
          InstrExtraBinop
            { _instrExtraBinopArg1 = arg1,
              _instrExtraBinopArg2 = arg2,
              _instrExtraBinopOpcode = op,
              _instrExtraBinopResult = res,
              _instrExtraBinopIncAp = incAp
            }

    parseExtraValue :: ExtraOpcode -> ParsecS r Value
    parseExtraValue = \case
      FieldSub -> Ref <$> parseMemRef
      FieldDiv -> Ref <$> parseMemRef
      _ -> parseValue

registerAP :: ParsecS r Reg
registerAP = do
  kw kwAp
  return Ap

registerFP :: ParsecS r Reg
registerFP = do
  kw kwFp
  return Fp

register :: ParsecS r Reg
register = registerAP <|> registerFP

opcode :: ParsecS r Opcode
opcode =
  (kw kwPlus >> return FieldAdd)
    <|> (kw kwMul >> return FieldMul)

extraOpcode :: ParsecS r ExtraOpcode
extraOpcode =
  (kw kwMinus >> return FieldSub)
    <|> (kw kwDiv >> return FieldDiv)
    <|> (kw kwIntAdd >> return IntAdd)
    <|> (kw kwIntSub >> return IntSub)
    <|> (kw kwIntMul >> return IntMul)
    <|> (kw kwIntDiv >> return IntDiv)
    <|> (kw kwIntMod >> return IntMod)
    <|> (kw kwIntLt >> return IntLt)
