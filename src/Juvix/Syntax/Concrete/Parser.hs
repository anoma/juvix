module Juvix.Syntax.Concrete.Parser
  ( module Juvix.Syntax.Concrete.Parser,
    module Juvix.Syntax.Concrete.Parser.ParserResult,
    module Juvix.Syntax.Concrete.Parser.InfoTable,
    module Juvix.Syntax.Concrete.Parser.Error,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Juvix.Pipeline.EntryPoint
import Juvix.Prelude
import Juvix.Prelude.Pretty (Pretty, prettyText)
import Juvix.Syntax.Concrete.Base qualified as P
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Lexer hiding (symbol)
import Juvix.Syntax.Concrete.Parser.Error
import Juvix.Syntax.Concrete.Parser.InfoTable
import Juvix.Syntax.Concrete.Parser.InfoTableBuilder
import Juvix.Syntax.Concrete.Parser.ParserResult

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

top ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r a ->
  ParsecS r a
top p = space >> p <* (optional kwSemicolon >> P.eof)

topModuleDef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = top moduleDef

topStatement ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Statement 'Parsed)
topStatement = top statement

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Symbol
symbol = uncurry (flip WithLoc) <$> identifierL

dottedSymbol :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry (flip WithLoc)) <$> dottedIdentifier

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
    <|> (StatementImport <$> import_)
    <|> (StatementInductive <$> inductiveDef Nothing)
    <|> (StatementForeign <$> foreignBlock)
    <|> (StatementModule <$> moduleDef)
    <|> (StatementAxiom <$> axiomDef Nothing)
    <|> (StatementCompile <$> compileBlock)
    <|> builtinStatement
    <|> ( either StatementTypeSignature StatementFunctionClause
            <$> auxTypeSigFunClause
        )

builtinInductive :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r BuiltinInductive
builtinInductive = builtinHelper

builtinFunction :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r BuiltinFunction
builtinFunction = builtinHelper

builtinAxiom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r BuiltinAxiom
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[Reader ParserParams, InfoTableBuilder] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r a
builtinHelper =
  P.choice
    [ keyword (prettyText a) $> a
      | a <- allElements
    ]

builtinInductiveDef :: Members '[Reader ParserParams, InfoTableBuilder] r => BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinTypeSig ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  BuiltinFunction ->
  ParsecS r (TypeSignature 'Parsed)
builtinTypeSig b = do
  fun <- symbol
  typeSignature False fun (Just b)

builtinStatement :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (Statement 'Parsed)
builtinStatement = do
  kwBuiltin
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementTypeSignature . builtinTypeSig)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

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
  AtomLiteral <$> P.try literal
    <|> (AtomIdentifier <$> name)
    <|> (AtomUniverse <$> universe)
    <|> (AtomLambda <$> lambda)
    <|> (AtomFunction <$> function)
    <|> (AtomLetBlock <$> letBlock)
    <|> (AtomFunArrow <$ kwRightArrow)
    <|> (AtomHole <$> hole)
    <|> parens (AtomParens <$> parseExpressionAtoms)
    <|> braces (AtomBraces <$> withLoc parseExpressionAtoms)

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
  return (WithLoc loc (LitInteger x))

literalString :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (WithLoc loc (LitString x))

literal :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

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
  i <- snd <$> interval kwType
  uni <- optional decimal
  return
    ( case uni of
        Nothing -> Universe Nothing i
        Just (lvl, i') -> Universe (Just lvl) (i <> i')
    )

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  Bool ->
  Symbol ->
  Maybe BuiltinFunction ->
  ParsecS r (TypeSignature 'Parsed)
typeSignature _sigTerminating _sigName _sigBuiltin = do
  kwColon
  _sigType <- parseExpressionAtoms
  return TypeSignature {..}

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  ParsecS r (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  terminating <- isJust <$> optional kwTerminating
  sym <- symbol
  (Left <$> typeSignature terminating sym Nothing)
    <|> (Right <$> functionClause sym)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef ::
  Members '[Reader ParserParams, InfoTableBuilder] r =>
  Maybe BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
axiomDef _axiomBuiltin = do
  kwAxiom
  _axiomName <- symbol
  kwColon
  _axiomType <- parseExpressionAtoms
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

implicitOpen :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r IsImplicit
implicitOpen =
  lbrace $> Implicit
    <|> lparen $> Explicit

implicitClose :: Members '[Reader ParserParams, InfoTableBuilder] r => IsImplicit -> ParsecS r ()
implicitClose = \case
  Implicit -> rbrace
  Explicit -> rparen

functionParam :: forall r. Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (FunctionParameter 'Parsed)
functionParam = do
  (_paramName, _paramUsage, _paramImplicit) <- P.try $ do
    impl <- implicitOpen
    n <- pName
    u <- pUsage
    return (n, u, impl)
  _paramType <- parseExpressionAtoms
  implicitClose _paramImplicit
  return FunctionParameter {..}
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

inductiveDef :: Members '[Reader ParserParams, InfoTableBuilder] r => Maybe BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
inductiveDef _inductiveBuiltin = do
  _inductivePositive <- isJust <$> optional kwPositive
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

wildcard :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval kwWildcard

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtom :: Members '[Reader ParserParams, InfoTableBuilder] r => ParsecS r (PatternAtom 'Parsed)
patternAtom =
  PatternAtomIden <$> name
    <|> PatternAtomWildcard <$> wildcard
    <|> (PatternAtomParens <$> parens parsePatternAtoms)
    <|> (PatternAtomBraces <$> braces parsePatternAtoms)

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
  _openModuleImport <- isJust <$> optional kwImport
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
