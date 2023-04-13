module Juvix.Compiler.Asm.Translation.FromSource
  ( module Juvix.Compiler.Asm.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: Text -> Either MegaparsecError InfoTable
parseText = runParser ""

runParser :: FilePath -> Text -> Either MegaparsecError InfoTable
runParser fileName input =
  case run $
    runInfoTableBuilder $
      evalTopNameIdGen $
        P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (MegaparsecError err)
    (tbl, Right ()) -> Right tbl

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
          _constructorInductive = sym,
          _constructorRepresentation = MemRepConstr
        }

declareBuiltins :: (Member InfoTableBuilder r) => ParsecS r ()
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
  (Member InfoTableBuilder r) =>
  ParsecS r ()
parseToplevel = do
  declareBuiltins
  space
  P.many statement
  P.eof

statement ::
  (Member InfoTableBuilder r) =>
  ParsecS r ()
statement = statementFunction <|> statementInductive

statementFunction ::
  (Member InfoTableBuilder r) =>
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
  argtys <- functionArguments
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
            _functionType = mkTypeFun argtys (fromMaybe TyDynamic mrty),
            _functionMaxValueStackHeight = -1, -- computed later
            _functionMaxTempStackHeight = -1
          }
  lift $ registerFunction fi0
  mcode <- (kw kwSemicolon $> Nothing) <|> optional (braces parseCode)
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
  (Member InfoTableBuilder r) =>
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
  ctrs <- braces $ P.sepEndBy (constrDecl sym) (kw kwSemicolon)
  lift $ registerInductive ii {_inductiveConstructors = map (^. constructorTag) ctrs}

functionArguments ::
  (Member InfoTableBuilder r) =>
  ParsecS r [Type]
functionArguments = do
  lparen
  args <- P.sepBy parseType comma
  rparen
  return args

constrDecl ::
  (Member InfoTableBuilder r) =>
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
  let ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = length (typeArgs ty'),
            _constructorType = ty',
            _constructorInductive = symInd,
            _constructorRepresentation = MemRepConstr
          }
  lift $ registerConstr ci
  return ci

typeAnnotation ::
  (Member InfoTableBuilder r) =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  parseType

parseType ::
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
  NonEmpty Type ->
  ParsecS r Type
typeFun' tyargs = do
  kw kwRightArrow
  TyFun . TypeFun tyargs <$> parseType

typeArguments ::
  (Member InfoTableBuilder r) =>
  ParsecS r (NonEmpty Type)
typeArguments = do
  parens (P.sepBy1 parseType comma <&> NonEmpty.fromList)
    <|> (typeDynamic <&> NonEmpty.singleton)
    <|> (typeNamed <&> NonEmpty.singleton)

typeDynamic :: ParsecS r Type
typeDynamic = kw kwStar $> TyDynamic

typeNamed ::
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
  ParsecS r Code
parseCode = P.sepEndBy command (kw kwSemicolon)

command ::
  (Member InfoTableBuilder r) =>
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
    "pusht" ->
      return $ mkInstr' loc PushTemp
    "popt" ->
      return $ mkInstr' loc PopTemp
    "trace" ->
      return $ mkInstr' loc Trace
    "dump" ->
      return $ mkInstr' loc Dump
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
    _ ->
      parseFailure off ("unknown instruction: " ++ fromText txt)

value ::
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
  ParsecS r Value
memValue = do
  r <- directRef
  parseField r <|> return (Ref (DRef r))

directRef :: ParsecS r DirectRef
directRef = stackRef <|> argRef <|> tempRef

stackRef :: ParsecS r DirectRef
stackRef = kw kwDollar $> StackRef

argRef :: ParsecS r DirectRef
argRef = do
  kw kwArg
  (off, _) <- brackets integer
  return $ ArgRef (fromInteger off)

tempRef :: ParsecS r DirectRef
tempRef = do
  kw kwTmp
  (off, _) <- brackets integer
  return $ TempRef (fromInteger off)

parseField ::
  (Member InfoTableBuilder r) =>
  DirectRef ->
  ParsecS r Value
parseField dref = do
  dot
  tag <- constrTag
  (off, _) <- brackets integer
  return $ Ref (ConstrRef (Field tag dref (fromInteger off)))

constrTag ::
  (Member InfoTableBuilder r) =>
  ParsecS r Tag
constrTag = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentConstr tag) -> return tag
    _ -> parseFailure off "expected a constructor"

indSymbol ::
  (Member InfoTableBuilder r) =>
  ParsecS r Symbol
indSymbol = do
  off <- P.getOffset
  txt <- identifier
  idt <- lift $ getIdent txt
  case idt of
    Just (IdentInd sym) -> return sym
    _ -> parseFailure off "expected an inductive type"

funSymbol ::
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
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
  (Member InfoTableBuilder r) =>
  ParsecS r CallType
parseCallType = (kw kwDollar $> CallClosure) <|> (CallFun <$> funSymbol)

instrCallClosures :: ParsecS r InstrCallClosures
instrCallClosures = do
  (argsNum, _) <- integer
  return (InstrCallClosures (fromInteger argsNum))

branchCode ::
  (Member InfoTableBuilder r) =>
  ParsecS r Code
branchCode = braces parseCode <|> (command >>= \x -> return [x])

trueBranch ::
  (Member InfoTableBuilder r) =>
  ParsecS r Code
trueBranch = do
  symbol "true:"
  branchCode

falseBranch ::
  (Member InfoTableBuilder r) =>
  ParsecS r Code
falseBranch = do
  symbol "false:"
  branchCode

caseBranch ::
  (Member InfoTableBuilder r) =>
  ParsecS r CaseBranch
caseBranch = do
  tag <- P.try constrTag
  kw kwColon
  CaseBranch tag <$> branchCode

defaultBranch ::
  (Member InfoTableBuilder r) =>
  ParsecS r Code
defaultBranch = symbol "default:" >> branchCode
