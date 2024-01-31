module Juvix.Compiler.Tree.Translation.FromSource
  ( module Juvix.Compiler.Tree.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.InfoTableBuilder
import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Translation.FromSource.Base
import Juvix.Compiler.Tree.Translation.FromSource.Lexer
import Juvix.Compiler.Tree.Translation.FromSource.Sig qualified as S
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type ParserSig = S.ParserSig Node ()

parseTreeSig :: ParserSig
parseTreeSig =
  S.ParserSig
    { _parserSigBareIdentifier = bareIdentifier,
      _parserSigParseCode = parseNode,
      _parserSigEmptyCode = mkUnop OpFail (mkConst (ConstString "fail")),
      _parserSigEmptyExtra = ()
    }

parseText :: Text -> Either MegaparsecError InfoTable
parseText = runParser ""

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs ""

runParser :: FilePath -> Text -> Either MegaparsecError InfoTable
runParser = runParserS parseTreeSig

runParser' :: BuilderState -> FilePath -> Text -> Either MegaparsecError BuilderState
runParser' = runParserS' parseTreeSig

parseNode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Node
parseNode =
  (Binop <$> parseBinop)
    <|> (Unop <$> parseUnop)
    <|> (Const <$> parseConst)
    <|> (AllocConstr <$> parseAlloc)
    <|> (AllocClosure <$> parseCAlloc)
    <|> (ExtendClosure <$> parseCExtend)
    <|> (Call <$> parseCall)
    <|> (CallClosures <$> parseCCall)
    <|> (Branch <$> parseBranch)
    <|> (Case <$> parseCase)
    <|> (Save <$> parseSave)
    <|> (MemRef <$> parseMemRef)

parseBinop ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeBinop
parseBinop =
  parseBinaryOp kwAdd_ IntAdd
    <|> parseBinaryOp kwSub_ IntSub
    <|> parseBinaryOp kwMul_ IntMul
    <|> parseBinaryOp kwDiv_ IntDiv
    <|> parseBinaryOp kwMod_ IntMod
    <|> parseBinaryOp kwLt_ IntLt
    <|> parseBinaryOp kwLe_ IntLe
    <|> parseBinaryOp kwEq_ ValEq
    <|> parseBinaryOp kwStrcat StrConcat
    <|> parseBinaryOp kwSeq_ OpSeq

parseBinaryOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  BinaryOpcode ->
  ParsecS r NodeBinop
parseBinaryOp kwd op = do
  loc <- onlyInterval (kw kwd)
  lparen
  arg1 <- parseNode
  comma
  arg2 <- parseNode
  rparen
  return $ NodeBinop (NodeInfo (Just loc)) op arg1 arg2

parseUnop ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeUnop
parseUnop =
  parseUnaryOp kwShow OpShow
    <|> parseUnaryOp kwAtoi OpStrToInt
    <|> parseUnaryOp kwTrace OpTrace
    <|> parseUnaryOp kwFail OpFail
    <|> parseUnaryOp kwArgsNum OpArgsNum

parseUnaryOp ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  Keyword ->
  UnaryOpcode ->
  ParsecS r NodeUnop
parseUnaryOp kwd op = do
  loc <- onlyInterval (kw kwd)
  arg <- parens parseNode
  return $ NodeUnop (NodeInfo (Just loc)) op arg

parseConst :: ParsecS r NodeConstant
parseConst = do
  (c, loc) <- interval constant
  return $ NodeConstant (NodeInfo (Just loc)) c

parseMemRef ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeMemRef
parseMemRef = do
  (r, loc) <- interval (memRef @Node @())
  return $ NodeMemRef (NodeInfo (Just loc)) r

parseArgs ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r [Node]
parseArgs = parens (P.sepBy parseNode comma)

parseAlloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeAllocConstr
parseAlloc = do
  loc <- onlyInterval (kw kwAlloc)
  tag <- brackets (constrTag @Node @())
  args <- parseArgs
  return
    NodeAllocConstr
      { _nodeAllocConstrInfo = NodeInfo (Just loc),
        _nodeAllocConstrTag = tag,
        _nodeAllocConstrArgs = args
      }

parseCAlloc ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeAllocClosure
parseCAlloc = do
  loc <- onlyInterval (kw kwCAlloc)
  sym <- brackets (funSymbol @Node @())
  args <- parseArgs
  return
    NodeAllocClosure
      { _nodeAllocClosureInfo = NodeInfo (Just loc),
        _nodeAllocClosureFunSymbol = sym,
        _nodeAllocClosureArgs = args
      }

parseCExtend ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeExtendClosure
parseCExtend = do
  loc <- onlyInterval (kw kwCExtend)
  off <- P.getOffset
  args <- parseArgs
  case args of
    arg1 : arg2 : args' ->
      return
        NodeExtendClosure
          { _nodeExtendClosureInfo = NodeInfo (Just loc),
            _nodeExtendClosureFun = arg1,
            _nodeExtendClosureArgs = arg2 :| args'
          }
    _ ->
      parseFailure off "expected at least two arguments"

parseCall ::
  forall r.
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeCall
parseCall = do
  loc <- onlyInterval (kw kwCall)
  callDirect loc <|> callClosure loc
  where
    callDirect :: Location -> ParsecS r NodeCall
    callDirect loc = do
      lbracket
      sym <- funSymbol @Node @()
      rbracket
      args <- parseArgs
      return
        NodeCall
          { _nodeCallInfo = NodeInfo (Just loc),
            _nodeCallType = CallFun sym,
            _nodeCallArgs = args
          }

    callClosure :: Location -> ParsecS r NodeCall
    callClosure loc = do
      off <- P.getOffset
      args <- parseArgs
      case args of
        arg : args' ->
          return
            NodeCall
              { _nodeCallInfo = NodeInfo (Just loc),
                _nodeCallType = CallClosure arg,
                _nodeCallArgs = args'
              }
        [] ->
          parseFailure off "expected at least one argument"

parseCCall ::
  forall r.
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeCallClosures
parseCCall = do
  loc <- onlyInterval (kw kwCCall)
  off <- P.getOffset
  args <- parseArgs
  case args of
    [_] ->
      parseFailure off "expected at least two arguments"
    arg : args' ->
      return
        NodeCallClosures
          { _nodeCallClosuresInfo = NodeInfo (Just loc),
            _nodeCallClosuresFun = arg,
            _nodeCallClosuresArgs = nonEmpty' args'
          }
    [] ->
      parseFailure off "expected at least two arguments"

parseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeBranch
parseBranch = do
  loc <- onlyInterval (kw kwBr)
  arg <- parens parseNode
  lbrace
  br1 <- trueBranch
  br2 <- falseBranch
  rbrace
  return
    NodeBranch
      { _nodeBranchInfo = NodeInfo (Just loc),
        _nodeBranchArg = arg,
        _nodeBranchTrue = br1,
        _nodeBranchFalse = br2
      }

branchNode ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Node
branchNode = braces parseNode <|> parseNode

trueBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Node
trueBranch = do
  symbol "true:"
  c <- branchNode
  optional (kw delimSemicolon)
  return c

falseBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Node
falseBranch = do
  symbol "false:"
  c <- branchNode
  optional (kw delimSemicolon)
  return c

parseCase ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeCase
parseCase = do
  loc <- onlyInterval (kw kwCase)
  sym <- brackets (indSymbol @Node @())
  arg <- parens parseNode
  lbrace
  brs <- P.many caseBranch
  def <- optional defaultBranch
  rbrace
  return
    NodeCase
      { _nodeCaseInfo = NodeInfo (Just loc),
        _nodeCaseInductive = sym,
        _nodeCaseArg = arg,
        _nodeCaseBranches = brs,
        _nodeCaseDefault = def
      }

caseBranch ::
  forall r.
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r CaseBranch
caseBranch = do
  (tag, loc) <- P.try $ interval (constrTag @Node @())
  kw kwColon
  (bSave, body) <- saveBranch <|> discardBranch
  optional (kw delimSemicolon)
  return
    CaseBranch
      { _caseBranchLocation = Just loc,
        _caseBranchTag = tag,
        _caseBranchBody = body,
        _caseBranchSave = bSave
      }
  where
    saveBranch :: ParsecS r (Bool, Node)
    saveBranch = do
      kw kwSave
      (True,) <$> braces parseNode

    discardBranch :: ParsecS r (Bool, Node)
    discardBranch = do
      (False,) <$> branchNode

defaultBranch ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r Node
defaultBranch = do
  symbol "default:"
  c <- branchNode
  optional (kw delimSemicolon)
  return c

parseSave ::
  (Members '[Reader ParserSig, InfoTableBuilder, State LocalParams] r) =>
  ParsecS r NodeSave
parseSave = do
  loc' <- onlyInterval (kw kwSave)
  (mname, loc) <- interval $ optional (brackets identifier)
  arg <- parens parseNode
  tmpNum <- lift $ gets (^. localParamsTempIndex)
  let updateNames :: LocalNameMap -> LocalNameMap
      updateNames mp = maybe mp (\n -> HashMap.insert n (mkTempRef (OffsetRef tmpNum (Just n))) mp) mname
  body <- braces (localS (over localParamsTempIndex (+ 1)) $ localS (over localParamsNameMap updateNames) parseNode)
  return
    NodeSave
      { _nodeSaveInfo = NodeInfo (Just loc'),
        _nodeSaveArg = arg,
        _nodeSaveBody = body,
        _nodeSaveTempVar = TempVar mname (Just loc)
      }
