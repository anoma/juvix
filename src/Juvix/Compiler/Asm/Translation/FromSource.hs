module Juvix.Compiler.Asm.Translation.FromSource
  ( module Juvix.Compiler.Asm.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Compiler.Tree.Translation.FromSource.Base
import Juvix.Compiler.Tree.Translation.FromSource.Sig qualified as S
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type ParserSig = S.ParserSig Code (Maybe FunctionInfoExtra)

parseAsmSig :: ParserSig
parseAsmSig =
  S.ParserSig
    { _parserSigBareIdentifier = bareIdentifier,
      _parserSigParseCode = parseCode,
      _parserSigEmptyCode = [],
      _parserSigEmptyExtra = mempty
    }

parseText :: Text -> Either MegaparsecError InfoTable
parseText = runParser ""

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs ""

runParser :: FilePath -> Text -> Either MegaparsecError InfoTable
runParser fileName input_ = (^. stateInfoTable) <$> runParser' emptyBuilderState fileName input_

runParser' :: BuilderState -> FilePath -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseAsmSig

parseCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
parseCode = P.sepEndBy command (kw delimSemicolon)

command ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Command
command = do
  off <- P.getOffset
  (txt, i) <- identifierL
  let loc = Just i
  case txt of
    "add" ->
      return $ mkBinop' loc IntAdd
    "sub" ->
      return $ mkBinop' loc IntSub
    "mul" ->
      return $ mkBinop' loc IntMul
    "div" ->
      return $ mkBinop' loc IntDiv
    "mod" ->
      return $ mkBinop' loc IntMod
    "lt" ->
      return $ mkBinop' loc IntLt
    "le" ->
      return $ mkBinop' loc IntLe
    "eq" ->
      return $ mkBinop' loc ValEq
    "strcat" ->
      return $ mkBinop' loc StrConcat
    "show" ->
      return $ mkInstr' loc ValShow
    "atoi" ->
      return $ mkInstr' loc StrToInt
    "push" ->
      mkInstr' loc . Push <$> value
    "pop" ->
      return $ mkInstr' loc Pop
    "trace" ->
      return $ mkInstr' loc Trace
    "dump" ->
      return $ mkInstr' loc Dump
    "fail" ->
      return $ mkInstr' loc Failure
    "argsnum" ->
      return $ mkInstr' loc ArgsNum
    "alloc" ->
      mkInstr' loc . AllocConstr <$> constrTag @Code @(Maybe FunctionInfoExtra)
    "calloc" ->
      mkInstr' loc . AllocClosure <$> instrAllocClosure
    "cextend" ->
      mkInstr' loc . ExtendClosure <$> instrExtendClosure
    "call" ->
      mkInstr' loc . Call <$> instrCall
    "tcall" ->
      mkInstr' loc . TailCall <$> instrCall
    "ccall" ->
      mkInstr' loc . CallClosures <$> instrCallClosures
    "tccall" ->
      mkInstr' loc . TailCallClosures <$> instrCallClosures
    "ret" ->
      return $ mkInstr' loc Return
    "br" -> do
      lbrace
      br1 <- trueBranch
      br2 <- falseBranch
      rbrace
      return $ Branch $ CmdBranch (CommandInfo loc) br1 br2
    "case" -> do
      sym <- indSymbol @Code @(Maybe FunctionInfoExtra)
      lbrace
      brs <- P.many caseBranch
      def <- optional defaultBranch
      rbrace
      return $ Case (CmdCase (CommandInfo loc) sym brs def)
    "save" ->
      parseSave loc False
    "tsave" ->
      parseSave loc True
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

parseSave ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Maybe Interval ->
  Bool ->
  ParsecS r Command
parseSave loc isTail = do
  mn <- optional identifier
  tmpNum <- lift $ gets (^. localParamsTempIndex)
  let updateNames :: LocalNameMap -> LocalNameMap
      updateNames mp = maybe mp (\n -> HashMap.insert n (mkTempRef (OffsetRef tmpNum (Just n))) mp) mn
  c <- braces (localS (over localParamsTempIndex (+ 1)) $ localS (over localParamsNameMap updateNames) parseCode)
  return $
    Save
      ( CmdSave
          { _cmdSaveInfo = CommandInfo loc,
            _cmdSaveIsTail = isTail,
            _cmdSaveCode = c,
            _cmdSaveName = mn
          }
      )

value ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Value
value = (Constant <$> constant) <|> (Ref <$> memRef @Code @(Maybe FunctionInfoExtra))

instrAllocClosure ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrAllocClosure
instrAllocClosure = do
  sym <- funSymbol @Code @(Maybe FunctionInfoExtra)
  (argsNum, _) <- integer
  return $ InstrAllocClosure sym (fromInteger argsNum)

instrExtendClosure :: ParsecS r InstrExtendClosure
instrExtendClosure = do
  (argsNum, _) <- integer
  return $ InstrExtendClosure (fromInteger argsNum)

instrCall ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r InstrCall
instrCall = do
  ct <- parseCallType
  argsNum <- case ct of
    CallFun sym -> do
      fi <- lift $ getFunctionInfo sym
      return (fi ^. functionArgsNum)
    CallClosure -> do
      (n, _) <- integer
      return (fromInteger n)
  return (InstrCall ct argsNum)

parseCallType ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CallType
parseCallType = (kw kwDollar $> CallClosure) <|> (CallFun <$> funSymbol @Code @(Maybe FunctionInfoExtra))

instrCallClosures :: ParsecS r InstrCallClosures
instrCallClosures = do
  (argsNum, _) <- integer
  return (InstrCallClosures (fromInteger argsNum))

branchCode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
branchCode = braces parseCode <|> (command >>= \x -> return [x])

trueBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
trueBranch = do
  symbol "true:"
  c <- branchCode
  kw delimSemicolon
  return c

falseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
falseBranch = do
  symbol "false:"
  c <- branchCode
  kw delimSemicolon
  return c

caseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try $ constrTag @Code @(Maybe FunctionInfoExtra)
  kw kwColon
  c <- CaseBranch tag <$> branchCode
  kw delimSemicolon
  return c

defaultBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Code
defaultBranch = do
  symbol "default:"
  c <- branchCode
  kw delimSemicolon
  return c
