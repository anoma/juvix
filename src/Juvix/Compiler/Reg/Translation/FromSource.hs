module Juvix.Compiler.Reg.Translation.FromSource
  ( module Juvix.Compiler.Reg.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Data.InfoTableBuilder
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Reg.Translation.FromSource.Lexer
import Juvix.Compiler.Tree.Translation.FromSource.Base
import Juvix.Compiler.Tree.Translation.FromSource.Sig qualified as S
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type ParserSig = S.ParserSig Code FunctionInfoExtra VarRef

type LocalParams = LocalParams' VarRef

parseAsmSig :: ParserSig
parseAsmSig =
  S.ParserSig
    { _parserSigBareIdentifier = bareIdentifier,
      _parserSigParseCode = parseCode,
      _parserSigArgRef = VarRef VarGroupArgs,
      _parserSigEmptyCode = [],
      _parserSigEmptyExtra = FunctionInfoExtra 0
    }

parseText :: Text -> Either MegaparsecError InfoTable
parseText = runParser ""

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs ""

runParser :: FilePath -> Text -> Either MegaparsecError InfoTable
runParser = runParserS parseAsmSig

runParser' :: BuilderState -> FilePath -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseAsmSig

parseCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
parseCode = P.sepEndBy instruction (kw delimSemicolon)

instruction ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Instruction
instruction =
  (instrNop >> return Nop)
    <|> (Trace <$> instrTrace)
    <|> (instrDump >> return Dump)
    <|> (Failure <$> instrFailure)
    <|> (Prealloc <$> instrPrealloc)
    <|> (Return <$> instrReturn)
    <|> (Branch <$> instrBranch)
    <|> (Case <$> instrCase)
    <|> (Block <$> instrBlock)
    <|> instrWithResult

instrWithResult ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Instruction
instrWithResult = do
  vref <- varRef
  kw kwEq
  (Binop <$> instrBinop vref)
    <|> (Show <$> instrShow vref)
    <|> (StrToInt <$> instrStrToInt vref)
    <|> (ArgsNum <$> instrArgsNum vref)
    <|> (Alloc <$> instrAlloc vref)
    <|> (AllocClosure <$> instrAllocClosure vref)
    <|> (ExtendClosure <$> instrExtendClosure vref)
    <|> (Call <$> instrCall vref)
    <|> (CallClosures <$> instrCallClosures vref)
    <|> (Assign <$> instrAssign vref)

instrNop :: ParsecS r ()
instrNop = kw kwNop

instrBinop ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r BinaryOp
instrBinop vref =
  parseBinaryOp kwAdd_ OpIntAdd vref
    <|> parseBinaryOp kwSub_ OpIntSub vref
    <|> parseBinaryOp kwMul_ OpIntMul vref
    <|> parseBinaryOp kwDiv_ OpIntDiv vref
    <|> parseBinaryOp kwMod_ OpIntMod vref
    <|> parseBinaryOp kwLt_ OpIntLt vref
    <|> parseBinaryOp kwLe_ OpIntLe vref
    <|> parseBinaryOp kwEq_ OpEq vref
    <|> parseBinaryOp kwStrcat OpStrConcat vref

parseBinaryOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  Opcode ->
  VarRef ->
  ParsecS r BinaryOp
parseBinaryOp kwd op vref = do
  kw kwd
  arg1 <- value
  comma
  arg2 <- value
  return $
    BinaryOp
      { _binaryOpCode = op,
        _binaryOpResult = vref,
        _binaryOpArg1 = arg1,
        _binaryOpArg2 = arg2
      }

instrShow ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrShow
instrShow = undefined

instrStrToInt ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrStrToInt
instrStrToInt = undefined

instrAssign ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAssign
instrAssign = undefined

instrTrace ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrTrace
instrTrace = undefined

instrDump ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r ()
instrDump = undefined

instrFailure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrFailure
instrFailure = undefined

instrArgsNum ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrArgsNum
instrArgsNum = undefined

instrPrealloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrPrealloc
instrPrealloc = undefined

instrAlloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAlloc
instrAlloc = undefined

instrAllocClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAllocClosure
instrAllocClosure = undefined

instrExtendClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrExtendClosure
instrExtendClosure = undefined

instrCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCall
instrCall = undefined

instrCallClosures ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCallClosures
instrCallClosures = undefined

instrReturn ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrReturn
instrReturn = undefined

instrBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBranch
instrBranch = undefined

instrCase ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrCase
instrCase = undefined

instrBlock ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBlock
instrBlock = undefined

varRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
varRef = varArg <|> varTmp <|> namedRef @Code @FunctionInfoExtra @VarRef

varArg :: ParsecS r VarRef
varArg = do
  kw kwArg
  off <- brackets int
  return
    VarRef
      { _varRefGroup = VarGroupArgs,
        _varRefIndex = off,
        _varRefName = Nothing
      }

varTmp :: ParsecS r VarRef
varTmp = do
  kw kwTmp
  off <- brackets int
  return
    VarRef
      { _varRefGroup = VarGroupLocal,
        _varRefIndex = off,
        _varRefName = Nothing
      }

value ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
value = (Const <$> constant) <|> varOrConstrRef

varOrConstrRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
varOrConstrRef = do
  r <- varRef
  (CRef <$> constrField r) <|> return (VRef r)

constrField ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r ConstrField
constrField ref = do
  dot
  tag <- constrTag @Code @FunctionInfoExtra @VarRef
  off <- brackets int
  return
    ConstrField
      { _constrFieldTag = tag,
        _constrFieldMemRep = MemRepConstr,
        _constrFieldRef = ref,
        _constrFieldIndex = off
      }
