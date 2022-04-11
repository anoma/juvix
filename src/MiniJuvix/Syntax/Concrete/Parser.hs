module MiniJuvix.Syntax.Concrete.Parser
  ( module MiniJuvix.Syntax.Concrete.Parser,
    module MiniJuvix.Syntax.Concrete.Parser.ParserResult,
    module MiniJuvix.Syntax.Concrete.Parser.InfoTable,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base (MonadParsec)
import MiniJuvix.Syntax.Concrete.Base qualified as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Lexer hiding (symbol)
import MiniJuvix.Syntax.Concrete.Parser.InfoTable
import MiniJuvix.Syntax.Concrete.Parser.InfoTableBuilder
import MiniJuvix.Syntax.Concrete.Parser.ParserResult

--------------------------------------------------------------------------------
-- Running the parser
--------------------------------------------------------------------------------

entryParser :: Members '[Files, Error Text] r => EntryPoint -> Sem r ParserResult
entryParser e = do
  (_resultTable, _resultModules) <- runInfoTableBuilder (mapM goFile (e ^. entryModulePaths))
  let _resultEntry = e
  return ParserResult {..}
  where
    goFile ::
      Members '[Files, Error Text, InfoTableBuilder] r =>
      FilePath ->
      Sem r (Module 'Parsed 'ModuleTop)
    goFile fileName = do
      input <- readFile' fileName
      case runModuleParser'' fileName input of
        Left er -> throw er
        Right (tbl, m) -> mergeTable tbl $> m

runModuleParserIO :: FilePath -> IO (Either Text (Module 'Parsed 'ModuleTop))
runModuleParserIO fileName =
  fmap (fmap snd) (runModuleParserIO' fileName)

runModuleParserIO' :: FilePath -> IO (Either Text (InfoTable, Module 'Parsed 'ModuleTop))
runModuleParserIO' fileName = do
  input <- Text.readFile fileName
  return (runModuleParser'' fileName input)

runModuleParser :: FilePath -> Text -> Either Text (Module 'Parsed 'ModuleTop)
runModuleParser fileName input = fmap snd (runModuleParser'' fileName input)

-- | The 'FilePath' is only used for reporting errors. It is safe to pass
-- an empty string.
runModuleParser'' :: FilePath -> Text -> Either Text (InfoTable, Module 'Parsed 'ModuleTop)
runModuleParser'' fileName input =
  case run $ runInfoTableBuilder $ P.runParserT topModuleDef fileName input of
    (_, Left err) -> Left (Text.pack (P.errorBundlePretty err))
    (tbl, Right r) -> return (tbl, r)

-- runModuleParser' :: FilePath -> Text -> Either Text ParserResult
-- runModuleParser' fileName input =
--   mkResult <$> runModuleParser'' fileName input
--   where
--   mkResult (t, m) = ParserResult {
--       _resultTable = t,
--       _resultModules = pure m
--       }

topModuleDef :: Member InfoTableBuilder r => ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = space >> moduleDef <* (optional kwSemicolon >> P.eof)

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: Member InfoTableBuilder r => ParsecS r Symbol
symbol = uncurry Symbol <$> identifierL

dottedSymbol :: Member InfoTableBuilder r => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry Symbol) <$> dottedIdentifier

name :: Member InfoTableBuilder r => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (Path p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: Member InfoTableBuilder r => ParsecS r (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol kwSemicolon)

topModulePath :: Member InfoTableBuilder r => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: Member InfoTableBuilder r => ParsecS r (Statement 'Parsed)
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
    <|> ( either StatementTypeSignature StatementFunctionClause
            <$> auxTypeSigFunClause
        )

--------------------------------------------------------------------------------
-- Foreign
--------------------------------------------------------------------------------

backend :: Member InfoTableBuilder r => ParsecS r Backend
backend =
  ghc $> BackendGhc
    <|> agda $> BackendAgda

foreignBlock :: Member InfoTableBuilder r => ParsecS r ForeignBlock
foreignBlock = do
  kwForeign
  _foreignBackend <- backend
  _foreignCode <- bracedString
  return ForeignBlock {..}

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: MonadParsec e Text m => m Precedence
precedence = PrecNat <$> decimal

operatorSyntaxDef :: forall r. Member InfoTableBuilder r => ParsecS r OperatorSyntaxDef
operatorSyntaxDef = do
  fixityArity <- arity
  fixityPrecedence <- precedence
  opSymbol <- symbol
  let opFixity = Fixity {..}
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

import_ :: Member InfoTableBuilder r => ParsecS r (Import 'Parsed)
import_ = do
  kwImport
  importModule <- topModulePath
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionAtom :: Member InfoTableBuilder r => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  do
    AtomLiteral <$> P.try literal
    <|> AtomIdentifier <$> name
    <|> (AtomUniverse <$> universe)
    <|> (AtomLambda <$> lambda)
    <|> (AtomFunction <$> function)
    <|> (AtomMatch <$> match)
    <|> (AtomLetBlock <$> letBlock)
    <|> (AtomFunArrow <$ kwRightArrow)
    <|> parens (AtomParens <$> expressionAtoms)

expressionAtoms :: Member InfoTableBuilder r => ParsecS r (ExpressionAtoms 'Parsed)
expressionAtoms = ExpressionAtoms <$> P.some expressionAtom

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: MonadParsec e Text m => m LiteralLoc
literalInteger = do
  (x, loc) <- interval integer
  return (LiteralLoc (LitInteger x) loc)

literalString :: MonadParsec e Text m => m LiteralLoc
literalString = do
  (x, loc) <- interval string
  return (LiteralLoc (LitString x) loc)

literal :: Member InfoTableBuilder r => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

matchAlt :: Member InfoTableBuilder r => ParsecS r (MatchAlt 'Parsed)
matchAlt = do
  matchAltPattern <- patternAtom
  kwMapsTo
  matchAltBody <- expressionAtoms
  return MatchAlt {..}

match :: Member InfoTableBuilder r => ParsecS r (Match 'Parsed)
match = do
  kwMatch
  matchExpression <- expressionAtoms
  matchAlts <- braces (P.sepEndBy matchAlt kwSemicolon)
  return Match {..}

--------------------------------------------------------------------------------
-- Let expression
--------------------------------------------------------------------------------

letClause :: Member InfoTableBuilder r => ParsecS r (LetClause 'Parsed)
letClause = do
  either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: Member InfoTableBuilder r => ParsecS r (LetBlock 'Parsed)
letBlock = do
  kwLet
  letClauses <- braces (P.sepEndBy letClause kwSemicolon)
  kwIn
  letExpression <- expressionAtoms
  return LetBlock {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: Member InfoTableBuilder r => ParsecS r Universe
universe = do
  kwType
  Universe <$> optional decimal

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature :: Member InfoTableBuilder r => Symbol -> ParsecS r (TypeSignature 'Parsed)
typeSignature _sigName = do
  kwColon
  _sigType <- expressionAtoms
  return TypeSignature {..}

-------------------------------------------------------------------------------
-- Aux type signature function clause
-------------------------------------------------------------------------------

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  Member InfoTableBuilder r =>
  ParsecS r (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  s <- symbol
  (Left <$> typeSignature s)
    <|> (Right <$> functionClause s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef :: Member InfoTableBuilder r => ParsecS r (AxiomDef 'Parsed)
axiomDef = do
  kwAxiom
  _axiomName <- symbol
  kwColon
  _axiomType <- expressionAtoms
  _axiomBackendItems <- fromMaybe [] <$> optional backends
  return AxiomDef {..}
  where
    backends = toList <$> braces (P.sepEndBy1 backendItem kwSemicolon)
    backendItem = do
      _backendItemBackend <- backend
      kwMapsTo
      _backendItemCode <- string
      return BackendItem {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

functionParam :: forall r. Member InfoTableBuilder r => ParsecS r (FunctionParameter 'Parsed)
functionParam = do
  (paramName, paramUsage) <- P.try $ do
    lparen
    n <- pName
    u <- pUsage
    return (n, u)
  paramType <- expressionAtoms
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

function :: Member InfoTableBuilder r => ParsecS r (Function 'Parsed)
function = do
  funParameter <- functionParam
  kwRightArrow
  funReturn <- expressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

whereBlock :: Member InfoTableBuilder r => ParsecS r (WhereBlock 'Parsed)
whereBlock = do
  kwWhere
  WhereBlock <$> braces (P.sepEndBy1 whereClause kwSemicolon)

whereClause :: forall r. Member InfoTableBuilder r => ParsecS r (WhereClause 'Parsed)
whereClause =
  (WhereOpenModule <$> openModule)
    <|> sigOrFun
  where
    sigOrFun :: ParsecS r (WhereClause 'Parsed)
    sigOrFun = either WhereTypeSig WhereFunClause <$> auxTypeSigFunClause

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: Member InfoTableBuilder r => ParsecS r (LambdaClause 'Parsed)
lambdaClause = do
  lambdaParameters <- P.some patternAtom
  kwMapsTo
  lambdaBody <- expressionAtoms
  return LambdaClause {..}

lambda :: Member InfoTableBuilder r => ParsecS r (Lambda 'Parsed)
lambda = do
  kwLambda
  lambdaClauses <- braces (P.sepEndBy lambdaClause kwSemicolon)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: Member InfoTableBuilder r => ParsecS r (InductiveDef 'Parsed)
inductiveDef = do
  kwInductive
  _inductiveName <- symbol
  _inductiveParameters <- P.many inductiveParam
  _inductiveType <- optional (kwColon >> expressionAtoms)
  _inductiveConstructors <- braces $ P.sepEndBy constructorDef kwSemicolon
  return InductiveDef {..}

inductiveParam :: Member InfoTableBuilder r => ParsecS r (InductiveParameter 'Parsed)
inductiveParam = parens $ do
  _inductiveParameterName <- symbol
  kwColon
  _inductiveParameterType <- expressionAtoms
  return InductiveParameter {..}

constructorDef :: Member InfoTableBuilder r => ParsecS r (InductiveConstructorDef 'Parsed)
constructorDef = do
  _constructorName <- symbol
  kwColon
  _constructorType <- expressionAtoms
  return InductiveConstructorDef {..}

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtom :: Member InfoTableBuilder r => ParsecS r (PatternAtom 'Parsed)
patternAtom =
  PatternAtomIden <$> name
    <|> PatternAtomWildcard <$ kwWildcard
    <|> (PatternAtomParens <$> parens patternAtoms)

patternAtoms :: Member InfoTableBuilder r => ParsecS r (PatternAtoms 'Parsed)
patternAtoms = PatternAtoms <$> P.some patternAtom

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause :: Member InfoTableBuilder r => Symbol -> ParsecS r (FunctionClause 'Parsed)
functionClause _clauseOwnerFunction = do
  _clausePatterns <- P.many patternAtom
  kwAssignment
  _clauseBody <- expressionAtoms
  _clauseWhere <- optional whereBlock
  return FunctionClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Member InfoTableBuilder r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: (SingI t, Member InfoTableBuilder r) => ParsecS r (Module 'Parsed t)
moduleDef = do
  kwModule
  _modulePath <- pmodulePath
  _moduleParameters <- many inductiveParam
  kwSemicolon
  _moduleBody <- P.sepEndBy statement kwSemicolon
  kwEnd
  return Module {..}

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: Member InfoTableBuilder r => ParsecS r (ExpressionType 'Parsed)
atomicExpression = do
  atom <- expressionAtom
  case atom of
    AtomFunArrow -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom)

openModule :: forall r. Member InfoTableBuilder r => ParsecS r (OpenModule 'Parsed)
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

eval :: Member InfoTableBuilder r => ParsecS r (Eval 'Parsed)
eval = do
  kwEval
  Eval <$> expressionAtoms

printS :: Member InfoTableBuilder r => ParsecS r (Print 'Parsed)
printS = do
  kwPrint
  Print <$> expressionAtoms
