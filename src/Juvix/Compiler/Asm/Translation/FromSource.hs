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
  Type ->
  Interval ->
  ConstructorInfo
createBuiltinConstr sym btag name ty i =
  let n = builtinConstrArgsNum btag
   in ConstructorInfo
        { _constructorName = name,
          _constructorLocation = Just i,
          _constructorTag = BuiltinTag btag,
          _constructorType = ty,
          _constructorArgsNum = n,
          _constructorInductive = sym
        }

declareBuiltins :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ()
declareBuiltins = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift freshSymbol
  let tyio = mkTypeInductive sym
  let constrs =
        [ createBuiltinConstr sym TagReturn "return" (mkTypeFun [TyDynamic] tyio) i,
          createBuiltinConstr sym TagBind "bind" (mkTypeFun [tyio, mkTypeFun [TyDynamic] tyio] tyio) i,
          createBuiltinConstr sym TagWrite "write" (mkTypeFun [TyDynamic] tyio) i,
          createBuiltinConstr sym TagReadLn "readLn" tyio i
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
statement = statementFunction <|> statementInductive

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
  when (txt == "main") $
    lift (registerMain sym)
  argtys <- functionArguments
  when (txt == "main" && not (null argtys)) $
    parseFailure off "the 'main' function must take zero arguments"
  mrty <- optional typeAnnotation
  let rty = fromMaybe TyDynamic mrty
  mcode <- optional (braces parseCode)
  let fi =
        FunctionInfo
          { _functionName = txt,
            _functionSymbol = sym,
            _functionLocation = Just i,
            _functionCode = fromMaybe [] mcode,
            _functionArgsNum = length argtys,
            _functionType = mkTypeFun argtys rty
          }
  case idt of
    Just (IdentFwd _) -> do
      when (isNothing mcode) $
        parseFailure off ("duplicate forward declaration of " ++ fromText txt)
      fi' <- lift $ getFunctionInfo sym
      unless
        ( fi' ^. functionArgsNum == fi ^. functionArgsNum
            && isSubtype (fi' ^. functionType) (fi ^. functionType)
        )
        $ parseFailure off "function definition does not match earlier declaration"
      lift $ registerFunction fi
    _ -> do
      lift $ registerFunction fi
      when (isNothing mcode) $
        lift (registerForward txt sym)

statementInductive ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r ()
statementInductive = do
  kwInductive
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  sym <- lift freshSymbol
  ctrs <- braces $ P.sepEndBy (constrDecl sym) kwSemicolon
  lift $
    registerInductive
      InductiveInfo
        { _inductiveName = txt,
          _inductiveLocation = Just i,
          _inductiveSymbol = sym,
          _inductiveKind = TyDynamic,
          _inductiveConstructors = ctrs
        }

functionArguments ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r [Type]
functionArguments = do
  lparen
  args <- P.sepBy parseType comma
  rparen
  return args

constrDecl ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  Symbol ->
  ParsecS r ConstructorInfo
constrDecl symInd = do
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  tag <- lift freshTag
  ty <- typeAnnotation
  let ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = length (typeArgs ty),
            _constructorType = ty,
            _constructorInductive = symInd
          }
  lift $ registerConstr ci
  return ci

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
    "br" -> do
      -- hlint is stupid here
      br1 <- trueBranch
      Branch . CmdBranch (CommandInfo loc) br1 <$> falseBranch
    "case" -> do
      sym <- indSymbol
      brs <- P.many caseBranch
      def <- optional defaultBranch
      return $ Case (CmdCase (CommandInfo loc) sym brs def)
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

value ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Value
value = integerValue <|> boolValue <|> stringValue <|> unitValue <|> voidValue <|> memValue

integerValue ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Value
integerValue = do
  (i, _) <- integer
  return $ ConstInt i

boolValue :: ParsecS r Value
boolValue =
  (kwTrue >> return (ConstBool True))
    <|> (kwFalse >> return (ConstBool False))

stringValue ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Value
stringValue = do
  (s, _) <- string
  return $ ConstString s

unitValue :: ParsecS r Value
unitValue = kwUnit >> return ConstUnit

voidValue :: ParsecS r Value
voidValue = kwVoid >> return ConstVoid

memValue ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Value
memValue = do
  r <- directRef
  parseField r <|> return (Ref (DRef r))

directRef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r DirectRef
directRef = stackRef <|> argRef <|> tempRef

stackRef :: ParsecS r DirectRef
stackRef = kwDollar >> return StackRef

argRef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r DirectRef
argRef = do
  kwArg
  (off, _) <- brackets integer
  return $ ArgRef (fromInteger off)

tempRef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r DirectRef
tempRef = do
  kwTmp
  (off, _) <- brackets integer
  return $ TempRef (fromInteger off)

parseField ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  DirectRef ->
  ParsecS r Value
parseField dref = do
  dot
  tag <- constrTag
  (off, _) <- brackets integer
  return $ Ref (ConstrRef (Field tag dref (fromInteger off)))

constrTag ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Tag
constrTag = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentConstr tag) -> return tag
    _ -> parseFailure off "expected a constructor"

indSymbol ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Symbol
indSymbol = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentInd sym) -> return sym
    _ -> parseFailure off "expected an inductive type"

funSymbol ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Symbol
funSymbol = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentFwd sym) -> return sym
    Just (IdentFun sym) -> return sym
    _ -> parseFailure off "expected a function"

instrAllocClosure ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrAllocClosure
instrAllocClosure = do
  sym <- funSymbol
  (argsNum, _) <- integer
  return $ InstrAllocClosure sym (fromInteger argsNum)

instrExtendClosure ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrExtendClosure
instrExtendClosure = do
  (argsNum, _) <- integer
  return $ InstrExtendClosure (fromInteger argsNum)

instrCall ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
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
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r CallType
parseCallType = (kwDollar >> return CallClosure) <|> (CallFun <$> funSymbol)

instrCallClosures ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r InstrCallClosures
instrCallClosures = do
  (argsNum, _) <- integer
  return (InstrCallClosures (fromInteger argsNum))

branchCode ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
branchCode = braces parseCode <|> (command >>= \x -> return [x])

trueBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
trueBranch = do
  symbol "true:"
  branchCode

falseBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
falseBranch = do
  symbol "false:"
  branchCode

caseBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try constrTag
  kwColon
  CaseBranch tag <$> branchCode

defaultBranch ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r Code
defaultBranch = symbol "default:" >> branchCode
