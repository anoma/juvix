module Juvix.Compiler.Asm.Translation.FromSource
  ( module Juvix.Compiler.Asm.Translation.FromSource,
    module Juvix.Parser.Error,
    BuilderState,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

type LocalNameMap = HashMap Text DirectRef

localS :: (Member (State s) r) => (s -> s) -> ParsecS r a -> ParsecS r a
localS update a = do
  s <- lift get
  lift $ put (update s)
  a' <- a
  lift $ put s
  return a'

parseText :: Text -> Either MegaparsecError InfoTable
parseText = runParser ""

parseText' :: BuilderState -> Text -> Either MegaparsecError BuilderState
parseText' bs = runParser' bs ""

runParser :: FilePath -> Text -> Either MegaparsecError InfoTable
runParser fileName input = (^. stateInfoTable) <$> runParser' emptyBuilderState fileName input

runParser' :: BuilderState -> FilePath -> Text -> Either MegaparsecError BuilderState
runParser' bs fileName input =
  case run $
    evalState @Index 0 $
      evalState @LocalNameMap mempty $
        runInfoTableBuilder' bs $
          evalTopNameIdGen defaultModuleId $
            P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (MegaparsecError err)
    (bs', Right ()) -> Right bs'

createBuiltinConstr ::
  Symbol ->
  BuiltinDataTag ->
  Text ->
  Type ->
  Location ->
  ConstructorInfo
createBuiltinConstr sym btag name ty i =
  let n = builtinConstrArgsNum btag
   in ConstructorInfo
        { _constructorName = name,
          _constructorLocation = Just i,
          _constructorTag = BuiltinTag btag,
          _constructorType = ty,
          _constructorArgsNum = n,
          _constructorArgNames = replicate n Nothing,
          _constructorInductive = sym,
          _constructorRepresentation = MemRepConstr,
          _constructorFixity = Nothing
        }

declareBuiltins :: (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) => ParsecS r ()
declareBuiltins = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift freshSymbol
  let tyio = mkTypeInductive sym
      constrs =
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
            _inductiveConstructors = map (^. constructorTag) constrs,
            _inductiveRepresentation = IndRepStandard
          }
      )
  lift $ mapM_ registerConstr constrs

parseToplevel ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r ()
parseToplevel = do
  declareBuiltins
  space
  P.many statement
  P.eof

statement ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r ()
statement = statementFunction <|> statementInductive

statementFunction ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r ()
statementFunction = do
  kw kwFun
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  sym <- case idt of
    Nothing -> lift freshSymbol
    Just (IdentFwd sym) -> return sym
    _ -> parseFailure off ("duplicate identifier: " ++ fromText txt)
  when (txt == "main") $
    lift (registerMain sym)
  args <- functionArguments
  let argtys = map snd args
      argnames = map fst args
  when (txt == "main" && not (null argtys)) $
    parseFailure off "the 'main' function must take zero arguments"
  mrty <- optional typeAnnotation
  let fi0 =
        FunctionInfo
          { _functionName = txt,
            _functionSymbol = sym,
            _functionLocation = Just i,
            _functionCode = [],
            _functionArgsNum = length argtys,
            _functionArgNames = argnames,
            _functionType = mkTypeFun argtys (fromMaybe TyDynamic mrty),
            _functionMaxValueStackHeight = -1, -- computed later
            _functionMaxTempStackHeight = -1
          }
  lift $ registerFunction fi0
  let updateNames :: LocalNameMap -> LocalNameMap
      updateNames names =
        foldr
          (\(mn, idx) h -> maybe h (\n -> HashMap.insert n (ArgRef (OffsetRef idx (Just n))) h) mn)
          names
          (zip argnames [0 ..])
  mcode <-
    (kw delimSemicolon $> Nothing)
      <|> optional (braces (localS updateNames parseCode))
  let fi = fi0 {_functionCode = fromMaybe [] mcode}
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
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r ()
statementInductive = do
  kw kwInductive
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  sym <- lift freshSymbol
  let ii =
        InductiveInfo
          { _inductiveName = txt,
            _inductiveLocation = Just i,
            _inductiveSymbol = sym,
            _inductiveKind = TyDynamic,
            _inductiveConstructors = [],
            _inductiveRepresentation = IndRepStandard
          }
  lift $ registerInductive ii
  ctrs <- braces $ P.sepEndBy (constrDecl sym) (kw delimSemicolon)
  lift $ registerInductive ii {_inductiveConstructors = map (^. constructorTag) ctrs}

functionArguments ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r [(Maybe Text, Type)]
functionArguments = do
  lparen
  args <- P.sepBy parseArgument comma
  rparen
  return args

constrDecl ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
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
  let ty' = uncurryType ty
      argsNum = length (typeArgs ty')
      ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = argsNum,
            _constructorArgNames = replicate argsNum Nothing,
            _constructorType = ty',
            _constructorInductive = symInd,
            _constructorRepresentation = MemRepConstr,
            _constructorFixity = Nothing
          }
  lift $ registerConstr ci
  return ci

typeAnnotation ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  parseType

parseArgument :: (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) => ParsecS r (Maybe Text, Type)
parseArgument = do
  n <- optional $ P.try $ do
    txt <- identifier
    kw kwColon
    return txt
  ty <- parseType
  return (n, ty)

parseType ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Type
parseType = do
  tys <- typeArguments
  off <- P.getOffset
  typeFun' tys
    <|> do
      unless (null (NonEmpty.tail tys)) $
        parseFailure off "expected \"->\""
      return (head tys)

typeFun' ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  NonEmpty Type ->
  ParsecS r Type
typeFun' tyargs = do
  kw kwRightArrow
  TyFun . TypeFun tyargs <$> parseType

typeArguments ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r (NonEmpty Type)
typeArguments = do
  parens (P.sepBy1 parseType comma <&> NonEmpty.fromList)
    <|> (typeDynamic <&> NonEmpty.singleton)
    <|> (typeNamed <&> NonEmpty.singleton)

typeDynamic :: ParsecS r Type
typeDynamic = kw kwStar $> TyDynamic

typeNamed ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Type
typeNamed = do
  off <- P.getOffset
  txt <- identifier
  case txt of
    "integer" -> return mkTypeInteger
    "bool" -> return mkTypeBool
    "string" -> return TyString
    "unit" -> return TyUnit
    _ -> do
      idt <- lift $ getIdent txt
      case idt of
        Just (IdentInd sym) -> return (mkTypeInductive sym)
        _ -> parseFailure off ("not a type: " ++ fromText txt)

parseCode ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Code
parseCode = P.sepEndBy command (kw delimSemicolon)

command ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
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
      lbrace
      br1 <- trueBranch
      br2 <- falseBranch
      rbrace
      return $ Branch $ CmdBranch (CommandInfo loc) br1 br2
    "case" -> do
      sym <- indSymbol
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
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  Maybe Interval ->
  Bool ->
  ParsecS r Command
parseSave loc isTail = do
  mn <- optional identifier
  tmpNum <- lift get
  let updateNames :: LocalNameMap -> LocalNameMap
      updateNames mp = maybe mp (\n -> HashMap.insert n (TempRef (OffsetRef tmpNum (Just n))) mp) mn
  c <- braces (localS @Index (+ 1) $ localS updateNames parseCode)
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
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Value
value = integerValue <|> boolValue <|> stringValue <|> unitValue <|> voidValue <|> memValue

integerValue :: ParsecS r Value
integerValue = do
  (i, _) <- integer
  return $ ConstInt i

boolValue :: ParsecS r Value
boolValue =
  (kw kwTrue $> ConstBool True)
    <|> (kw kwFalse $> ConstBool False)

stringValue :: ParsecS r Value
stringValue = do
  (s, _) <- string
  return $ ConstString s

unitValue :: ParsecS r Value
unitValue = kw kwUnit $> ConstUnit

voidValue :: ParsecS r Value
voidValue = kw kwVoid $> ConstVoid

memValue ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Value
memValue = do
  r <- directRef
  parseField r <|> return (Ref (DRef r))

directRef :: (Member (State LocalNameMap) r) => ParsecS r DirectRef
directRef = stackRef <|> argRef <|> tempRef <|> namedRef

stackRef :: ParsecS r DirectRef
stackRef = kw kwDollar $> StackRef

argRef :: ParsecS r DirectRef
argRef = do
  kw kwArg
  (off, _) <- brackets integer
  return $ ArgRef (OffsetRef (fromInteger off) Nothing)

tempRef :: ParsecS r DirectRef
tempRef = do
  kw kwTmp
  (off, _) <- brackets integer
  return $ TempRef (OffsetRef (fromInteger off) Nothing)

namedRef :: (Member (State LocalNameMap) r) => ParsecS r DirectRef
namedRef = do
  off <- P.getOffset
  txt <- identifier
  mr <- lift $ gets (HashMap.lookup txt)
  case mr of
    Just r -> return r
    Nothing -> parseFailure off "undeclared identifier"

parseField ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  DirectRef ->
  ParsecS r Value
parseField dref = do
  dot
  tag <- constrTag
  (off, _) <- brackets integer
  return $ Ref (ConstrRef (Field Nothing tag dref (fromInteger off)))

constrTag ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Tag
constrTag = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentConstr tag) -> return tag
    _ -> parseFailure off "expected a constructor"

indSymbol ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Symbol
indSymbol = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentInd sym) -> return sym
    _ -> parseFailure off "expected an inductive type"

funSymbol ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
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
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r InstrAllocClosure
instrAllocClosure = do
  sym <- funSymbol
  (argsNum, _) <- integer
  return $ InstrAllocClosure sym (fromInteger argsNum)

instrExtendClosure :: ParsecS r InstrExtendClosure
instrExtendClosure = do
  (argsNum, _) <- integer
  return $ InstrExtendClosure (fromInteger argsNum)

instrCall ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
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
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r CallType
parseCallType = (kw kwDollar $> CallClosure) <|> (CallFun <$> funSymbol)

instrCallClosures :: ParsecS r InstrCallClosures
instrCallClosures = do
  (argsNum, _) <- integer
  return (InstrCallClosures (fromInteger argsNum))

branchCode ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Code
branchCode = braces parseCode <|> (command >>= \x -> return [x])

trueBranch ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Code
trueBranch = do
  symbol "true:"
  c <- branchCode
  kw delimSemicolon
  return c

falseBranch ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Code
falseBranch = do
  symbol "false:"
  c <- branchCode
  kw delimSemicolon
  return c

caseBranch ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try constrTag
  kw kwColon
  c <- CaseBranch tag <$> branchCode
  kw delimSemicolon
  return c

defaultBranch ::
  (Members '[InfoTableBuilder, State LocalNameMap, State Index] r) =>
  ParsecS r Code
defaultBranch = do
  symbol "default:"
  c <- branchCode
  kw delimSemicolon
  return c
