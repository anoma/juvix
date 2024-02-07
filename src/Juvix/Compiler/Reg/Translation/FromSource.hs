module Juvix.Compiler.Reg.Translation.FromSource
  ( module Juvix.Compiler.Reg.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
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

parseRegSig :: ParserSig
parseRegSig =
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
runParser = runParserS parseRegSig

runParser' :: BuilderState -> FilePath -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseRegSig

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
  vref <- declVarRef
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
instrShow vref = do
  kw kwShow
  val <- value
  return
    InstrShow
      { _instrShowResult = vref,
        _instrShowValue = val
      }

instrStrToInt ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrStrToInt
instrStrToInt vref = do
  kw kwAtoi
  val <- value
  return
    InstrStrToInt
      { _instrStrToIntResult = vref,
        _instrStrToIntValue = val
      }

instrAssign ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAssign
instrAssign vref = do
  val <- value
  return
    InstrAssign
      { _instrAssignResult = vref,
        _instrAssignValue = val
      }

instrTrace ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrTrace
instrTrace = do
  kw kwTrace
  val <- value
  return
    InstrTrace
      { _instrTraceValue = val
      }

instrDump :: ParsecS r ()
instrDump = kw kwDump

instrFailure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrFailure
instrFailure = do
  kw kwFail
  val <- value
  return
    InstrFailure
      { _instrFailureValue = val
      }

instrArgsNum ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrArgsNum
instrArgsNum vref = do
  kw kwArgsNum
  val <- value
  return
    InstrArgsNum
      { _instrArgsNumResult = vref,
        _instrArgsNumValue = val
      }

instrPrealloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrPrealloc
instrPrealloc = do
  kw kwPrealloc
  n <- int
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrPrealloc
      { _instrPreallocWordsNum = n,
        _instrPreallocLiveVars = vars
      }

instrAlloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAlloc
instrAlloc vref = do
  kw kwAlloc
  tag <- constrTag @Code @FunctionInfoExtra @VarRef
  args <- parseArgs
  return
    InstrAlloc
      { _instrAllocResult = vref,
        _instrAllocTag = tag,
        _instrAllocArgs = args,
        _instrAllocMemRep = MemRepConstr
      }

instrAllocClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrAllocClosure
instrAllocClosure vref = do
  kw kwCAlloc
  sym <- funSymbol @Code @FunctionInfoExtra @VarRef
  args <- parseArgs
  fi <- lift $ getFunctionInfo sym
  return
    InstrAllocClosure
      { _instrAllocClosureResult = vref,
        _instrAllocClosureSymbol = sym,
        _instrAllocClosureExpectedArgsNum = fi ^. functionArgsNum,
        _instrAllocClosureArgs = args
      }

instrExtendClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrExtendClosure
instrExtendClosure vref = do
  kw kwCExtend
  cl <- varRef
  args <- parseArgs
  return
    InstrExtendClosure
      { _instrExtendClosureResult = vref,
        _instrExtendClosureValue = cl,
        _instrExtendClosureArgs = args
      }

liveVars ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r [VarRef]
liveVars = do
  P.try (kw kwLive >> kw kwColon)
  brackets (many varRef)

parseArgs ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r [Value]
parseArgs = do
  brackets (many value)

parseCallType ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CallType
parseCallType =
  (CallFun <$> funSymbol @Code @FunctionInfoExtra @VarRef)
    <|> (CallClosure <$> varRef)

instrCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCall
instrCall vref = do
  isTail <- (kw kwCall >> return False) <|> (kw kwCallTail >> return True)
  ct <- parseCallType
  args <- parseArgs
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrCall
      { _instrCallResult = vref,
        _instrCallType = ct,
        _instrCallIsTail = isTail,
        _instrCallArgs = args,
        _instrCallLiveVars = vars
      }

instrCallClosures ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  VarRef ->
  ParsecS r InstrCallClosures
instrCallClosures vref = do
  isTail <- (kw kwCCall >> return False) <|> (kw kwCCallTail >> return True)
  val <- varRef
  args <- parseArgs
  vars <- fromMaybe [] <$> optional liveVars
  return
    InstrCallClosures
      { _instrCallClosuresResult = vref,
        _instrCallClosuresIsTail = isTail,
        _instrCallClosuresValue = val,
        _instrCallClosuresArgs = args,
        _instrCallClosuresLiveVars = vars
      }

instrReturn ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrReturn
instrReturn = do
  kw kwRet
  val <- value
  return
    InstrReturn
      { _instrReturnValue = val
      }

instrBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBranch
instrBranch = do
  kw kwBr
  val <- value
  (br1, br2) <- braces $ do
    symbol "true:"
    br1 <- braces parseCode
    symbol "false:"
    br2 <- braces parseCode
    return (br1, br2)
  return
    InstrBranch
      { _instrBranchValue = val,
        _instrBranchTrue = br1,
        _instrBranchFalse = br2
      }

instrCase ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrCase
instrCase = do
  kw kwCase
  sym <- brackets (indSymbol @Code @FunctionInfoExtra @VarRef)
  val <- value
  lbrace
  brs <- many caseBranch
  def <- optional defaultBranch
  rbrace
  return
    InstrCase
      { _instrCaseValue = val,
        _instrCaseInductive = sym,
        _instrCaseIndRep = IndRepStandard,
        _instrCaseBranches = brs,
        _instrCaseDefault = def
      }

caseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- constrTag @Code @FunctionInfoExtra @VarRef
  kw kwColon
  body <- braces parseCode
  ci <- lift $ getConstructorInfo tag
  return
    CaseBranch
      { _caseBranchTag = tag,
        _caseBranchArgsNum = ci ^. constructorArgsNum,
        _caseBranchCode = body,
        _caseBranchMemRep = MemRepConstr
      }

defaultBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
defaultBranch = do
  symbol "default:"
  braces parseCode

instrBlock ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrBlock
instrBlock = InstrBlock <$> braces parseCode

varRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
varRef = varArg <|> varTmp <|> namedRef @Code @FunctionInfoExtra @VarRef

declVarRef ::
  forall r.
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r VarRef
declVarRef = varArg <|> varTmp <|> namedRef' @Code @FunctionInfoExtra @VarRef decl
  where
    decl :: Int -> Text -> ParsecS r VarRef
    decl _ txt = do
      idx <- lift $ gets @LocalParams (^. localParamsTempIndex)
      lift $ modify' @LocalParams (over localParamsTempIndex (+ 1))
      let vref = VarRef VarGroupLocal idx (Just txt)
      lift $ modify' (over localParamsNameMap (HashMap.insert txt vref))
      return vref

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