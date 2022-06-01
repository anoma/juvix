module MiniJuvix.Syntax.Concrete.Parser
  ( module MiniJuvix.Syntax.Concrete.Parser,
    module MiniJuvix.Syntax.Concrete.Parser.ParserResult,
    module MiniJuvix.Syntax.Concrete.Parser.InfoTable,
    module MiniJuvix.Syntax.Concrete.Parser.Error,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base qualified as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Lexer hiding (symbol)
import MiniJuvix.Syntax.Concrete.Parser.Error
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder
import MiniJuvix.Syntax.Concrete.Parser.ParserResult

--------------------------------------------------------------------------------
-- Running the parser
--------------------------------------------------------------------------------

entryParser ::
  Members '[Files, Error ParserError] r =>
  EntryPoint ->
  Sem r ParserResult
entryParser e = do
  (_resultTable, _resultModules) <- runInfoTableBuilder (runReader e (mapM goFile (e ^. entryPointModulePaths)))
  let _resultEntry = e
  return ParserResult {..}
  where
    goFile ::
      Members '[Files, Error ParserError, InfoTableBuilder] r =>
      FilePath ->
      Sem r (Module 'Parsed 'ModuleTop)
    goFile fileName = do
      input <- readFile' fileName
      case runModuleParser (e ^. entryPointRoot) fileName input of
        Left er -> throw er
        Right (tbl, m) -> mergeTable tbl $> m

-- | The fileName is only used for reporting errors. It is safe to pass
-- an empty string.
runModuleParser :: FilePath -> FilePath -> Text -> Either ParserError (InfoTable, Module 'Parsed 'ModuleTop)
runModuleParser root fileName input =
  case run $ runInfoTableBuilder $ runReader params $ P.runParserT topModuleDef fileName input of
    (_, Left err) -> Left (ParserError err)
    (tbl, Right r) -> return (tbl, r)
  where
    params =
      ParserParams
        { _parserParamsRoot = root
        }

topModuleDef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = space >> moduleDef <* (optional kwSemicolon >> P.eof)

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Symbol
symbol = uncurry Symbol <$> identifierL

dottedSymbol :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry Symbol) <$> dottedIdentifier

name :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (Path p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol kwSemicolon)

topModulePath :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Statement 'Parsed)
statement =
  (StatementOperator <$> operatorSyntaxDef)
    <|> (StatementOpenModule <$> openModule)
    <|> (StatementEval <$> eval)
    <|> (StatementImport <$> import_)
    <|> (StatementInductive <$> inductiveDef)
    <|> (StatementPrint <$> printS)
    <|> (StatementForeign <$> foreignBlock)
    <|> (StatementModule <$> moduleDef)
    <|> (StatementAxiom <$> axiomDef)
    <|> (StatementCompile <$> compileBlock)
    <|> ( either StatementTypeSignature StatementFunctionClause
            <$> auxTypeSigFunClause
        )

--------------------------------------------------------------------------------
-- Compile
--------------------------------------------------------------------------------

compileBlock :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Compile 'Parsed)
compileBlock = do
  kwCompile
  _compileName <- symbol
  _compileBackendItems <- backends
  return Compile {..}
  where
    backends = toList <$> braces (P.sepEndBy1 backendItem kwSemicolon)
    backendItem :: ParsecS r BackendItem
    backendItem = do
      _backendItemBackend <- backend
      kwMapsTo
      _backendItemCode <- fst <$> string
      return BackendItem {..}

--------------------------------------------------------------------------------
-- Foreign
--------------------------------------------------------------------------------

backend :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Backend
backend = ghc $> BackendGhc <|> cBackend $> BackendC

foreignBlock :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r ForeignBlock
foreignBlock = do
  kwForeign
  _foreignBackend <- backend
  _foreignCode <- bracedString
  return ForeignBlock {..}

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Precedence
precedence = PrecNat <$> (fst <$> decimal)

operatorSyntaxDef :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r OperatorSyntaxDef
operatorSyntaxDef = do
  _fixityArity <- arity
  _fixityPrecedence <- precedence
  _opSymbol <- symbol
  let _opFixity = Fixity {..}
  return OperatorSyntaxDef {..}
  where
    arity :: ParsecS r OperatorArity
    arity =
      do
        Binary AssocRight <$ kwInfixr
        <|> Binary AssocLeft <$ kwInfixl
        <|> Binary AssocNone <$ kwInfix
        <|> Unary AssocPostfix <$ kwPostfix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Import 'Parsed)
import_ = do
  kwImport
  _importModule <- topModulePath
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionAtom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  do AtomLiteral <$> P.try literal
    <|> (AtomIdentifier <$> name)
    <|> (AtomUniverse <$> universe)
    <|> (AtomLambda <$> lambda)
    <|> (AtomFunction <$> function)
    <|> (AtomMatch <$> match)
    <|> (AtomLetBlock <$> letBlock)
    <|> (AtomFunArrow <$ kwRightArrow)
    <|> (AtomHole <$> hole)
    <|> parens (AtomParens <$> parseExpressionAtoms)

parseExpressionAtoms ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- interval (P.some expressionAtom)
  return ExpressionAtoms {..}

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (HoleType 'Parsed)
hole = snd <$> interval kwHole

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r LiteralLoc
literalInteger = do
  (x, loc) <- integer
  return (LiteralLoc (LitInteger x) loc)

literalString :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (LiteralLoc (LitString x) loc)

literal :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

matchAlt :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (MatchAlt 'Parsed)
matchAlt = do
  matchAltPattern <- patternAtom
  kwMapsTo
  matchAltBody <- parseExpressionAtoms
  return MatchAlt {..}

match :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Match 'Parsed)
match = do
  kwMatch
  matchExpression <- parseExpressionAtoms
  matchAlts <- braces (P.sepEndBy matchAlt kwSemicolon)
  return Match {..}

--------------------------------------------------------------------------------
-- Let expression
--------------------------------------------------------------------------------

letClause :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (LetClause 'Parsed)
letClause = either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (LetBlock 'Parsed)
letBlock = do
  kwLet
  _letClauses <- braces (P.sepEndBy letClause kwSemicolon)
  kwIn
  _letExpression <- parseExpressionAtoms
  return LetBlock {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Universe
universe = do
  kwType
  Universe <$> optional (fst <$> decimal)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  Bool ->
  Symbol ->
  ParsecS r (TypeSignature 'Parsed)
typeSignature _sigTerminating _sigName = do
  kwColon
  _sigType <- parseExpressionAtoms
  return TypeSignature {..}

-------------------------------------------------------------------------------
-- Aux type signature function clause
-------------------------------------------------------------------------------

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  terminating <- isJust <$> optional kwTerminating
  sym <- symbol
  (Left <$> typeSignature terminating sym)
    <|> (Right <$> functionClause sym)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (AxiomDef 'Parsed)
axiomDef = do
  kwAxiom
  _axiomName <- symbol
  kwColon
  _axiomType <- parseExpressionAtoms
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

functionParam :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (FunctionParameter 'Parsed)
functionParam = do
  (_paramName, _paramUsage) <- P.try $ do
    lparen
    n <- pName
    u <- pUsage
    return (n, u)
  _paramType <- parseExpressionAtoms
  rparen
  return $ FunctionParameter {..}
  where
    pName :: ParsecS r (Maybe Symbol)
    pName =
      (Just <$> symbol)
        <|> (Nothing <$ kwWildcard)
    pUsage :: ParsecS r (Maybe Usage)
    pUsage =
      (Just UsageNone <$ kwColonZero)
        <|> (Just UsageOnce <$ kwColonOne)
        <|> (Just UsageOmega <$ kwColonOmega)
        <|> (Nothing <$ kwColon)

function :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Function 'Parsed)
function = do
  _funParameter <- functionParam
  kwRightArrow
  _funReturn <- parseExpressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

whereBlock :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (WhereBlock 'Parsed)
whereBlock = do
  kwWhere
  WhereBlock <$> braces (P.sepEndBy1 whereClause kwSemicolon)

whereClause :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (WhereClause 'Parsed)
whereClause =
  (WhereOpenModule <$> openModule)
    <|> sigOrFun
  where
    sigOrFun :: ParsecS r (WhereClause 'Parsed)
    sigOrFun = either WhereTypeSig WhereFunClause <$> auxTypeSigFunClause

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (LambdaClause 'Parsed)
lambdaClause = do
  lambdaParameters <- P.some patternAtom
  kwMapsTo
  lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Lambda 'Parsed)
lambda = do
  kwLambda
  _lambdaClauses <- braces (P.sepEndBy lambdaClause kwSemicolon)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (InductiveDef 'Parsed)
inductiveDef = do
  kwInductive
  _inductiveName <- symbol
  _inductiveParameters <- P.many inductiveParam
  _inductiveType <- optional (kwColon >> parseExpressionAtoms)
  _inductiveConstructors <- braces $ P.sepEndBy constructorDef kwSemicolon
  return InductiveDef {..}

inductiveParam :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (InductiveParameter 'Parsed)
inductiveParam = parens $ do
  _inductiveParameterName <- symbol
  kwColon
  _inductiveParameterType <- parseExpressionAtoms
  return InductiveParameter {..}

constructorDef :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (InductiveConstructorDef 'Parsed)
constructorDef = do
  _constructorName <- symbol
  kwColon
  _constructorType <- parseExpressionAtoms
  return InductiveConstructorDef {..}

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (PatternAtom 'Parsed)
patternAtom =
  PatternAtomIden <$> name
    <|> PatternAtomWildcard <$ kwWildcard
    <|> (PatternAtomParens <$> parens parsePatternAtoms)

parsePatternAtoms :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- interval (P.some patternAtom)
  return PatternAtoms {..}

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause :: Members '[Reader ParserParams, InfoTableBuilder] r => Symbol -> ParsecS r (FunctionClause 'Parsed)
functionClause _clauseOwnerFunction = do
  _clausePatterns <- P.many patternAtom
  kwAssignment
  _clauseBody <- parseExpressionAtoms
  _clauseWhere <- optional whereBlock
  return FunctionClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Members '[Reader ParserParams, InfoTableBuilder] r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: (SingI t, Members '[Reader ParserParams, InfoTableBuilder] r) => ParsecS r (Module 'Parsed t)
moduleDef = do
  kwModule
  _modulePath <- pmodulePath
  _moduleParameters <- many inductiveParam
  kwSemicolon
  _moduleBody <- P.sepEndBy statement kwSemicolon
  kwEnd
  return Module {..}

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (ExpressionAtoms 'Parsed)
atomicExpression = do
  (atom, loc) <- interval expressionAtom
  case atom of
    AtomFunArrow -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom) loc

openModule :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (OpenModule 'Parsed)
openModule = do
  kwOpen
  _openModuleName <- name
  _openParameters <- many atomicExpression
  _openUsingHiding <- optional usingOrHiding
  _openPublic <- maybe NoPublic (const Public) <$> optional kwPublic
  return OpenModule {..}
  where
    usingOrHiding :: ParsecS r UsingHiding
    usingOrHiding =
      (kwUsing >> (Using <$> symbolList))
        <|> (kwHiding >> (Hiding <$> symbolList))

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

eval :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Eval 'Parsed)
eval = do
  kwEval
  Eval <$> parseExpressionAtoms

printS :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Print 'Parsed)
printS = do
  kwPrint
  Print <$> parseExpressionAtoms
