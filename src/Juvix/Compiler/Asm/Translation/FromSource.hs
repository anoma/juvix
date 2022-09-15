module Juvix.Compiler.Asm.Translation.FromSource
  ( module Juvix.Compiler.Asm.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Control.Monad.Trans.Class (lift)
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type
import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: Text -> Either ParserError InfoTable
parseText = runParser "" ""

runParser :: FilePath -> FilePath -> Text -> Either ParserError InfoTable
runParser root fileName input =
  case run $
    runInfoTableBuilder $
      runReader params $
        runNameIdGen $
          P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (ParserError err)
    (tbl, Right ()) -> Right tbl
  where
    params =
      ParserParams
        { _parserParamsRoot = root
        }

createBuiltinConstr ::
  Symbol ->
  BuiltinDataTag ->
  Text ->
  Interval ->
  ConstructorInfo
createBuiltinConstr sym btag name i =
  let n = builtinConstrArgsNum btag
   in ConstructorInfo
        { _constructorName = name,
          _constructorLocation = Just i,
          _constructorTag = BuiltinTag btag,
          _constructorType = mkTypeFun (replicate n TyDynamic) (mkTypeInductive sym),
          _constructorArgsNum = n,
          _constructorInductive = sym
        }

declareBuiltins :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
declareBuiltins = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift freshSymbol
  let constrs =
        [ createBuiltinConstr sym TagReturn "return" i,
          createBuiltinConstr sym TagBind "bind" i,
          createBuiltinConstr sym TagWrite "write" i,
          createBuiltinConstr sym TagReadLn "readLn" i
        ]
  lift $
    registerInductive
      ( InductiveInfo
          { _inductiveName = "IO",
            _inductiveSymbol = sym,
            _inductiveLocation = Just i,
            _inductiveKind = TyDynamic,
            _inductiveConstructors = constrs
          }
      )
  lift $ mapM_ registerConstr constrs

parseToplevel ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
parseToplevel = do
  declareBuiltins
  space
  P.many statement
  P.eof

statement ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
statement = statementForward <|> statementFunction <|> statementInductive

statementForward ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
statementForward = undefined

statementFunction ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
statementFunction = do
  kwFun
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  sym <- case idt of
    Nothing -> lift freshSymbol
    Just (IdentFwd sym) -> return sym
    _ -> parseFailure off ("duplicate identifier: " ++ fromText txt)
  args <- functionArguments
  mrty <- optional typeAnnotation
  let rty = fromMaybe TyDynamic mrty
  code <- braces parseCode
  lift $ registerFunction
    FunctionInfo {
      _functionName = txt,
      _functionSymbol = sym,
      _functionLocation = Just i,
      _functionCode = code,
      _functionArgsNum = length args,
      _functionType = mkTypeFun (map snd args) rty
    }

statementInductive ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
statementInductive = undefined

functionArguments ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r [(Text, Type)]
functionArguments = do
  lparen
  args <- P.sepBy functionArgument comma
  rparen
  return args

functionArgument ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Text, Type)
functionArgument = do
  txt <- identifier
  mty <- optional typeAnnotation
  return (txt, fromMaybe TyDynamic mty)

typeAnnotation ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Type
typeAnnotation = do
  kwColon
  parseType

parseType ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Type
parseType = do
  ty <- typeDynamic <|> typeNamed
  typeFun' ty <|> return ty

typeFun' ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  Type ->
  ParsecS r Type
typeFun' ty = do
  kwArrow
  TyFun ty <$> parseType

typeDynamic :: ParsecS r Type
typeDynamic = kwStar >> return TyDynamic

typeNamed ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Type
typeNamed = do
  off <- P.getOffset
  txt <- identifier
  case txt of
    "integer" -> return mkInteger
    "bool" -> return mkBool
    "string" -> return TyString
    _ -> do
      idt <- lift $ getIdent txt
      case idt of
        Just (IdentInd sym) -> return (mkTypeInductive sym)
        _ -> parseFailure off ("not a type: " ++ fromText txt)

parseCode ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
parseCode = P.sepEndBy command kwSemicolon

command ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Command
command = do
  off <- P.getOffset
  (txt, i) <- identifierL
  let loc = Just i
  case txt of
    "add" ->
      return $ mkInstr' loc IntAdd
    "sub" ->
      return $ mkInstr' loc IntSub
    "mul" ->
      return $ mkInstr' loc IntMul
    "div" ->
      return $ mkInstr' loc IntDiv
    "lt" ->
      return $ mkInstr' loc IntLt
    "le" ->
      return $ mkInstr' loc IntLe
    "eq" ->
      return $ mkInstr' loc ValEq
    "push" ->
      mkInstr' loc . Push <$> value
    "pop" ->
      return $ mkInstr' loc Pop
    "pusht" ->
      return $ mkInstr' loc PushTemp
    "popt" ->
      return $ mkInstr' loc PopTemp
    "trace" ->
      return $ mkInstr' loc Trace
    "fail" ->
      return $ mkInstr' loc Failure
    "alloc" ->
      mkInstr' loc . AllocConstr <$> constrTag
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
    "br" -> do -- hlint is stupid here
      br1 <- trueBranch
      Branch . CmdBranch (CommandInfo loc) br1 <$> falseBranch
    "case" -> do
      brs <- P.many caseBranch
      def <- optional defaultBranch
      return $ Case (CmdCase (CommandInfo loc) brs def)
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

value ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Value
value = undefined

constrTag ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Tag
constrTag = undefined

instrAllocClosure ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrAllocClosure
instrAllocClosure = undefined

instrExtendClosure ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrExtendClosure
instrExtendClosure = undefined

instrCall ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrCall
instrCall = undefined

instrCallClosures ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrCallClosures
instrCallClosures = undefined

trueBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
trueBranch = undefined

falseBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
falseBranch = undefined

caseBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r CaseBranch
caseBranch = undefined

defaultBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
defaultBranch = undefined
