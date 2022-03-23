module MiniJuvix.Syntax.Concrete.Parser where

--------------------------------------------------------------------------------

import qualified Data.List.NonEmpty.Extra as NonEmpty
import Data.Singletons
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Base (MonadParsec)
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Lexer hiding (symbol)

--------------------------------------------------------------------------------
-- Running the parser
--------------------------------------------------------------------------------

debugModuleParser :: FilePath -> IO ()
debugModuleParser fileName = do
  r <- runModuleParserIO fileName
  case r of
    Left err -> error err
    Right m -> print m

runModuleParserIO :: FilePath -> IO (Either Text (Module 'Parsed 'ModuleTop))
runModuleParserIO fileName = do
  input <- Text.readFile fileName
  return (runModuleParser fileName input)

-- | The 'FilePath' is only used for reporting errors. It is safe to pass
-- an empty string.
runModuleParser :: FilePath -> Text -> Either Text (Module 'Parsed 'ModuleTop)
runModuleParser fileName input =
  case P.runParser topModuleDef fileName input of
    Left err -> Left $ Text.pack (P.errorBundlePretty err)
    Right r -> return r

topModuleDef :: MonadParsec Void Text m => m (Module 'Parsed 'ModuleTop)
topModuleDef = space >> moduleDef <* (optional kwSemicolon >> P.eof)

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: MonadParsec e Text m => m Symbol
symbol = uncurry Symbol <$> identifierL

dottedSymbol :: forall e m. MonadParsec e Text m => m (NonEmpty Symbol)
dottedSymbol = fmap (uncurry Symbol) <$> dottedIdentifier

name :: forall e m. MonadParsec e Text m => m Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (Path p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: MonadParsec e Text m => m (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol kwSemicolon)

topModulePath :: MonadParsec e Text m => m TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: forall e m. MonadParsec e Text m => m (Statement 'Parsed)
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

backend :: forall e m. MonadParsec e Text m => m Backend
backend = ghc $> BackendGhc
  <|> agda $> BackendAgda

foreignBlock :: forall e m. MonadParsec e Text m => m ForeignBlock
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

operatorSyntaxDef :: forall e m. MonadParsec e Text m => m OperatorSyntaxDef
operatorSyntaxDef = do
  fixityArity <- arity
  fixityPrecedence <- precedence
  opSymbol <- symbol
  let opFixity = Fixity {..}
  return OperatorSyntaxDef {..}
  where
    arity :: m OperatorArity
    arity =
      do
        Binary AssocRight <$ kwInfixr
        <|> Binary AssocLeft <$ kwInfixl
        <|> Binary AssocNone <$ kwInfix
        <|> Unary AssocPostfix <$ kwPostfix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: MonadParsec e Text m => m (Import 'Parsed)
import_ = do
  kwImport
  importModule <- topModulePath
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionAtom :: MonadParsec e Text m => m (ExpressionAtom 'Parsed)
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

expressionAtoms :: MonadParsec e Text m => m (ExpressionAtoms 'Parsed)
expressionAtoms = ExpressionAtoms <$> P.some expressionAtom

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literal :: MonadParsec e Text m => m Literal
literal =
  LitInteger <$> integer
    <|> LitString <$> string

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

matchAlt :: MonadParsec e Text m => m (MatchAlt 'Parsed)
matchAlt = do
  matchAltPattern <- patternAtom
  kwMapsTo
  matchAltBody <- expressionAtoms
  return MatchAlt {..}

match :: MonadParsec e Text m => m (Match 'Parsed)
match = do
  kwMatch
  matchExpression <- expressionAtoms
  matchAlts <- braces (P.sepEndBy matchAlt kwSemicolon)
  return Match {..}

--------------------------------------------------------------------------------
-- Let expression
--------------------------------------------------------------------------------

letClause :: MonadParsec e Text m => m (LetClause 'Parsed)
letClause = do
  either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: MonadParsec e Text m => m (LetBlock 'Parsed)
letBlock = do
  kwLet
  letClauses <- braces (P.sepEndBy letClause kwSemicolon)
  kwIn
  letExpression <- expressionAtoms
  return LetBlock {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: MonadParsec e Text m => m Universe
universe = do
  kwType
  Universe <$> optional decimal

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature ::
  forall e m.
  MonadParsec e Text m =>
  Symbol ->
  m (TypeSignature 'Parsed)
typeSignature _sigName = do
  kwColon
  _sigType <- expressionAtoms
  return TypeSignature {..}

-------------------------------------------------------------------------------
-- Aux type signature function clause
-------------------------------------------------------------------------------

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  forall e m.
  MonadParsec e Text m =>
  m (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  s <- symbol
  (Left <$> typeSignature s)
    <|> (Right <$> functionClause s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef :: forall e m. MonadParsec e Text m => m (AxiomDef 'Parsed)
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

functionParam ::
  forall e m.
  MonadParsec e Text m =>
  m (FunctionParameter 'Parsed)
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
    pName :: m (Maybe Symbol)
    pName =
      (Just <$> symbol)
        <|> (Nothing <$ kwWildcard)
    pUsage :: m (Maybe Usage)
    pUsage =
      (Just UsageNone <$ kwColonZero)
        <|> (Just UsageOnce <$ kwColonOne)
        <|> (Just UsageOmega <$ kwColonOmega)
        <|> (Nothing <$ kwColon)

function :: MonadParsec e Text m => m (Function 'Parsed)
function = do
  funParameter <- functionParam
  kwRightArrow
  funReturn <- expressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

whereBlock :: MonadParsec e Text m => m (WhereBlock 'Parsed)
whereBlock = do
  kwWhere
  WhereBlock <$> braces (P.sepEndBy1 whereClause kwSemicolon)

whereClause :: forall e m. MonadParsec e Text m => m (WhereClause 'Parsed)
whereClause =
  (WhereOpenModule <$> openModule)
    <|> sigOrFun
  where
    sigOrFun :: m (WhereClause 'Parsed)
    sigOrFun = either WhereTypeSig WhereFunClause <$> auxTypeSigFunClause

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: MonadParsec e Text m => m (LambdaClause 'Parsed)
lambdaClause = do
  lambdaParameters <- P.some patternAtom
  kwMapsTo
  lambdaBody <- expressionAtoms
  return LambdaClause {..}

lambda :: MonadParsec e Text m => m (Lambda 'Parsed)
lambda = do
  kwLambda
  lambdaClauses <- braces (P.sepEndBy lambdaClause kwSemicolon)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: MonadParsec e Text m => m (InductiveDef 'Parsed)
inductiveDef = do
  kwInductive
  _inductiveName <- symbol
  _inductiveParameters <- P.many inductiveParam
  _inductiveType <- optional (kwColon >> expressionAtoms)
  _inductiveConstructors <- braces $ P.sepEndBy constructorDef kwSemicolon
  return InductiveDef {..}

inductiveParam :: MonadParsec e Text m => m (InductiveParameter 'Parsed)
inductiveParam = parens $ do
  _inductiveParameterName <- symbol
  kwColon
  _inductiveParameterType <- expressionAtoms
  return InductiveParameter {..}

constructorDef :: MonadParsec e Text m => m (InductiveConstructorDef 'Parsed)
constructorDef = do
  constructorName <- symbol
  kwColon
  constructorType <- expressionAtoms
  return InductiveConstructorDef {..}

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtom :: forall e m. MonadParsec e Text m => m (PatternAtom 'Parsed)
patternAtom =
  PatternAtomIden <$> name
    <|> PatternAtomWildcard <$ kwWildcard
    <|> (PatternAtomParens <$> parens patternAtoms)

patternAtoms ::
  forall e m.
  MonadParsec e Text m =>
  m (PatternAtoms 'Parsed)
patternAtoms = PatternAtoms <$> P.some patternAtom

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause ::
  forall e m.
  MonadParsec e Text m =>
  Symbol ->
  m (FunctionClause 'Parsed)
functionClause _clauseOwnerFunction = do
  _clausePatterns <- P.many patternAtom
  kwAssignment
  _clauseBody <- expressionAtoms
  _clauseWhere <- optional whereBlock
  return FunctionClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath ::
  forall t e m.
  (SingI t, MonadParsec e Text m) =>
  m (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: (SingI t, MonadParsec e Text m) => m (Module 'Parsed t)
moduleDef = do
  kwModule
  _modulePath <- pmodulePath
  _moduleParameters <- many inductiveParam
  kwSemicolon
  _moduleBody <- P.sepEndBy statement kwSemicolon
  kwEnd
  return Module {..}

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: forall e m. MonadParsec e Text m => m (ExpressionType 'Parsed)
atomicExpression = do
  atom <- expressionAtom
  case atom of
    AtomFunArrow -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom)

openModule :: forall e m. MonadParsec e Text m => m (OpenModule 'Parsed)
openModule = do
  kwOpen
  openModuleName <- name
  openParameters <- many atomicExpression
  openUsingHiding <- optional usingOrHiding
  openPublic <- maybe NoPublic (const Public) <$> optional kwPublic
  return OpenModule {..}
  where
    usingOrHiding :: m UsingHiding
    usingOrHiding =
      (kwUsing >> (Using <$> symbolList))
        <|> (kwHiding >> (Hiding <$> symbolList))

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

eval :: MonadParsec e Text m => m (Eval 'Parsed)
eval = do
  kwEval
  Eval <$> expressionAtoms

printS :: MonadParsec e Text m => m (Print 'Parsed)
printS = do
  kwPrint
  Print <$> expressionAtoms
