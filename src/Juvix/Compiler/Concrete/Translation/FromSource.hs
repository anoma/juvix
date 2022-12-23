module Juvix.Compiler.Concrete.Translation.FromSource
  ( module Juvix.Compiler.Concrete.Translation.FromSource,
    module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Compiler.Concrete.Data.ParsedInfoTable,
    module Juvix.Parser.Error,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
import Juvix.Compiler.Concrete.Extra (MonadParsec (takeWhile1P))
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer hiding (symbol)
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Parser.Error
import Juvix.Prelude
import Juvix.Prelude.Pretty (Pretty, prettyText)

type JudocStash = State (Maybe (Judoc 'Parsed))

fromSource ::
  Members '[Files, Error JuvixError, NameIdGen] r =>
  EntryPoint ->
  Sem r ParserResult
fromSource e = mapError (JuvixError @ParserError) $ do
  (_resultTable, _resultModules) <- runInfoTableBuilder (runReader e (mapM goFile (e ^. entryPointModulePaths)))
  let _resultEntry = e
  return ParserResult {..}
  where
    goFile ::
      forall r.
      Members '[Files, Error ParserError, InfoTableBuilder, NameIdGen] r =>
      Path Abs File ->
      Sem r (Module 'Parsed 'ModuleTop)
    goFile fileName = do
      input <- getFileContents fileName
      mp <- runModuleParser fileName input
      case mp of
        Left er -> throw er
        Right (tbl, m) -> mergeTable tbl $> m
      where
        getFileContents :: Path Abs File -> Sem r Text
        getFileContents fp
          | fp == e ^. mainModulePath,
            Just txt <- e ^. entryPointStdin =
              return txt
          | otherwise = readFile' fp

-- | The fileName is only used for reporting errors. It is safe to pass
-- an empty string.
expressionFromTextSource ::
  Members '[Error JuvixError, NameIdGen] r =>
  FilePath ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionFromTextSource fp txt = mapError (JuvixError @ParserError) $ do
  exp <- runExpressionParser fp txt
  case exp of
    Left e -> throw e
    Right exp' -> return exp'

-- | The fileName is only used for reporting errors. It is safe to pass
-- an empty string.
runModuleParser :: Members '[NameIdGen] r => Path Abs File -> Text -> Sem r (Either ParserError (InfoTable, Module 'Parsed 'ModuleTop))
runModuleParser fileName input = do
  m <-
    runInfoTableBuilder $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT topModuleDef (toFilePath fileName) input
  case m of
    (_, Left err) -> return (Left (ParserError err))
    (tbl, Right r) -> return (Right (tbl, r))

-- | The fileName is only used for reporting errors. It is safe to pass
-- an empty string.
runExpressionParser ::
  Members '[NameIdGen] r =>
  FilePath ->
  Text ->
  Sem r (Either ParserError (ExpressionAtoms 'Parsed))
runExpressionParser fileName input = do
  m <-
    runInfoTableBuilder $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT parseExpressionAtoms fileName input
  case m of
    (_, Left err) -> return (Left (ParserError err))
    (_, Right r) -> return (Right r)

top ::
  Member InfoTableBuilder r =>
  ParsecS r a ->
  ParsecS r a
top p = space >> p <* (optional (kw kwSemicolon) >> P.eof)

topModuleDef ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = do
  void (optional stashJudoc)
  top moduleDef

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Symbol
symbol = uncurry (flip WithLoc) <$> identifierL

dottedSymbol :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry (flip WithLoc)) <$> dottedIdentifier

name :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (SymbolPath p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol (kw kwSemicolon))

topModulePath :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Statement 'Parsed)
statement = P.label "<top level statement>" $ do
  void (optional stashJudoc)
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

stashJudoc :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r ()
stashJudoc = do
  b <- judocBlocks
  many judocEmptyLine
  P.lift (modify (<> Just b))
  where
    judocBlocks :: ParsecS r (Judoc 'Parsed)
    judocBlocks = Judoc <$> some judocBlock
    judocBlock :: ParsecS r (JudocBlock 'Parsed)
    judocBlock = comment $ do
      p <-
        judocExample
          <|> judocParagraph

      void (many judocEmptyLine)
      return p
    judocParagraph :: ParsecS r (JudocBlock 'Parsed)
    judocParagraph = JudocParagraph <$> some1 judocLine

    judocExample :: ParsecS r (JudocBlock 'Parsed)
    judocExample = do
      P.try (judocStart >> judocExampleStart)
      uid <- P.lift freshNameId
      e <- parseExpressionAtoms
      kw kwSemicolon
      space
      return (JudocExample (Example uid e))

    judocLine :: ParsecS r (JudocParagraphLine 'Parsed)
    judocLine = lexeme $ do
      P.try (judocStart >> P.notFollowedBy (P.choice [judocExampleStart, void P.newline]))
      ln <- JudocParagraphLine <$> some1 judocAtom
      P.newline
      return ln

judocAtom :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (JudocAtom 'Parsed)
judocAtom =
  JudocText <$> judocText
    <|> JudocExpression <$> judocExpression
  where
    judocText :: ParsecS r Text
    judocText = comment (takeWhile1P Nothing isValidText)
      where
        isValidText :: Char -> Bool
        isValidText = (`notElem` ['\n', ';'])
    judocExpression :: ParsecS r (ExpressionAtoms 'Parsed)
    judocExpression = do
      comment_ (P.char ';')
      e <- parseExpressionAtoms
      comment_ (P.char ';')
      return e

builtinInductive :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r BuiltinInductive
builtinInductive = builtinHelper

builtinFunction :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r BuiltinFunction
builtinFunction = builtinHelper

builtinAxiom :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r BuiltinAxiom
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[InfoTableBuilder, JudocStash, NameIdGen] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r a
builtinHelper =
  P.choice
    [ kw (asciiKw (prettyText a)) $> a
      | a <- allElements
    ]

builtinInductiveDef :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinTypeSig ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  BuiltinFunction ->
  ParsecS r (TypeSignature 'Parsed)
builtinTypeSig b = do
  fun <- symbol
  typeSignature False fun (Just b)

builtinStatement :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Statement 'Parsed)
builtinStatement = do
  kw kwBuiltin
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementTypeSignature . builtinTypeSig)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

--------------------------------------------------------------------------------
-- Compile
--------------------------------------------------------------------------------

compileBlock :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Compile 'Parsed)
compileBlock = do
  kw kwCompile
  _compileName <- symbol
  _compileBackendItems <- backends
  return Compile {..}
  where
    backends = toList <$> braces (P.sepEndBy1 backendItem (kw kwSemicolon))
    backendItem :: ParsecS r BackendItem
    backendItem = do
      _backendItemBackend <- backend
      kw kwMapsTo
      _backendItemCode <- fst <$> string
      return BackendItem {..}

--------------------------------------------------------------------------------
-- Foreign
--------------------------------------------------------------------------------

backend :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Backend
backend = kw ghc $> BackendGhc <|> kw cBackend $> BackendC

foreignBlock :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r ForeignBlock
foreignBlock = do
  kw kwForeign
  _foreignBackend <- backend
  _foreignCode <- bracedString
  return ForeignBlock {..}

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Precedence
precedence = PrecNat <$> (fst <$> decimal)

operatorSyntaxDef :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r OperatorSyntaxDef
operatorSyntaxDef = do
  _fixityArity <- arity
  _fixityPrecedence <- precedence
  _opSymbol <- symbol
  let _opFixity = Fixity {..}
  return OperatorSyntaxDef {..}
  where
    arity :: ParsecS r OperatorArity
    arity =
      Binary AssocRight <$ kw kwInfixr
        <|> Binary AssocLeft <$ kw kwInfixl
        <|> Binary AssocNone <$ kw kwInfix
        <|> Unary AssocPostfix <$ kw kwPostfix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Import 'Parsed)
import_ = do
  kw kwImport
  _importModule <- topModulePath
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionAtom :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  P.label "<expression>" $
    AtomLiteral <$> P.try literal
      <|> (AtomIdentifier <$> name)
      <|> (AtomUniverse <$> universe)
      <|> (AtomLambda <$> lambda)
      <|> (AtomFunction <$> function)
      <|> (AtomLetBlock <$> letBlock)
      <|> (AtomFunArrow <$ kw kwRightArrow)
      <|> (AtomHole <$> hole)
      <|> parens (AtomParens <$> parseExpressionAtoms)
      <|> braces (AtomBraces <$> withLoc parseExpressionAtoms)

parseExpressionAtoms ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- interval (P.some expressionAtom)
  return ExpressionAtoms {..}

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (HoleType 'Parsed)
hole = snd <$> interval (kw kwHole)

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r LiteralLoc
literalInteger = do
  (x, loc) <- integer
  return (WithLoc loc (LitInteger x))

literalString :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (WithLoc loc (LitString x))

literal :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

letClause :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (LetClause 'Parsed)
letClause = either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (LetBlock 'Parsed)
letBlock = do
  kw kwLet
  _letClauses <- braces (P.sepEndBy letClause (kw kwSemicolon))
  kw kwIn
  _letExpression <- parseExpressionAtoms
  return LetBlock {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Universe
universe = do
  i <- snd <$> interval (kw kwType)
  uni <- optional decimal
  return
    ( case uni of
        Nothing -> Universe Nothing i
        Just (lvl, i') -> Universe (Just lvl) (i <> i')
    )

getJudoc :: Members '[JudocStash, NameIdGen] r => ParsecS r (Maybe (Judoc 'Parsed))
getJudoc = P.lift $ do
  j <- get
  put (Nothing @(Judoc 'Parsed))
  return j

typeSignature ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  Bool ->
  Symbol ->
  Maybe BuiltinFunction ->
  ParsecS r (TypeSignature 'Parsed)
typeSignature _sigTerminating _sigName _sigBuiltin = do
  kw kwColon
  _sigType <- parseExpressionAtoms
  _sigDoc <- getJudoc
  return TypeSignature {..}

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  ParsecS r (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  terminating <- isJust <$> optional (kw kwTerminating)
  sym <- symbol
  (Left <$> typeSignature terminating sym Nothing)
    <|> (Right <$> functionClause sym)

axiomDef ::
  Members '[InfoTableBuilder, JudocStash, NameIdGen] r =>
  Maybe BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
axiomDef _axiomBuiltin = do
  kw kwAxiom
  _axiomDoc <- getJudoc
  _axiomName <- symbol
  kw kwColon
  _axiomType <- parseExpressionAtoms
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

implicitOpen :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r IsImplicit
implicitOpen =
  lbrace $> Implicit
    <|> lparen $> Explicit

implicitClose :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => IsImplicit -> ParsecS r ()
implicitClose = \case
  Implicit -> rbrace
  Explicit -> rparen

functionParam :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (FunctionParameter 'Parsed)
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
        <|> (Nothing <$ kw kwWildcard)
    pUsage :: ParsecS r (Maybe Usage)
    pUsage =
      (Just UsageNone <$ kw kwColonZero)
        <|> (Just UsageOnce <$ kw kwColonOne)
        <|> (Just UsageOmega <$ kw kwColonOmega)
        <|> (Nothing <$ kw kwColon)

function :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Function 'Parsed)
function = do
  _funParameter <- functionParam
  kw kwRightArrow
  _funReturn <- parseExpressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (LambdaClause 'Parsed)
lambdaClause = do
  _lambdaParameters <- P.some patternAtom
  kw kwAssign
  _lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (Lambda 'Parsed)
lambda = do
  kw kwLambda
  _lambdaClauses <- braces (P.sepEndBy lambdaClause (kw kwSemicolon))
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => Maybe BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
inductiveDef _inductiveBuiltin = do
  _inductivePositive <- isJust <$> optional (kw kwPositive)
  kw kwInductive
  _inductiveDoc <- getJudoc
  _inductiveName <- symbol
  _inductiveParameters <- P.many inductiveParam
  _inductiveType <- optional (kw kwColon >> parseExpressionAtoms)
  _inductiveConstructors <- braces $ P.sepEndBy constructorDef (kw kwSemicolon)
  return InductiveDef {..}

inductiveParam :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (InductiveParameter 'Parsed)
inductiveParam = parens $ do
  _inductiveParameterName <- symbol
  kw kwColon
  _inductiveParameterType <- parseExpressionAtoms
  return InductiveParameter {..}

constructorDef :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (InductiveConstructorDef 'Parsed)
constructorDef = do
  _constructorDoc <- optional stashJudoc >> getJudoc
  _constructorName <- symbol
  kw kwColon
  _constructorType <- parseExpressionAtoms
  return InductiveConstructorDef {..}

wildcard :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval (kw kwWildcard)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtomAnon :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (PatternAtom 'Parsed)
patternAtomAnon =
  PatternAtomWildcard <$> wildcard
    <|> PatternAtomParens <$> parens parsePatternAtoms
    <|> PatternAtomBraces <$> braces parsePatternAtoms

patternAtomAt :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => SymbolType 'Parsed -> ParsecS r (PatternAtom 'Parsed)
patternAtomAt s = kw kwAt >> PatternAtomAt . PatternBinding s <$> patternAtom

patternAtomNamed :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (PatternAtom 'Parsed)
patternAtomNamed = do
  n <- name
  case n of
    NameQualified _ -> return (PatternAtomIden n)
    NameUnqualified s -> patternAtomAt s <|> return (PatternAtomIden n)

patternAtom :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (PatternAtom 'Parsed)
patternAtom = P.label "<pattern>" $ patternAtomNamed <|> patternAtomAnon

parsePatternAtoms :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- interval (P.some patternAtom)
  return PatternAtoms {..}

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => Symbol -> ParsecS r (FunctionClause 'Parsed)
functionClause _clauseOwnerFunction = do
  _clausePatterns <- P.many patternAtom
  kw kwAssign
  _clauseBody <- parseExpressionAtoms
  return FunctionClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Members '[InfoTableBuilder, JudocStash, NameIdGen] r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: (SingI t, Members '[InfoTableBuilder, JudocStash, NameIdGen] r) => ParsecS r (Module 'Parsed t)
moduleDef = P.label "<module definition>" $ do
  kw kwModule
  _moduleDoc <- getJudoc
  _modulePath <- pmodulePath
  _moduleParameters <- many inductiveParam
  kw kwSemicolon
  _moduleBody <- P.sepEndBy statement (kw kwSemicolon)
  kw kwEnd
  return Module {..}

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (ExpressionAtoms 'Parsed)
atomicExpression = do
  (atom, loc) <- interval expressionAtom
  case atom of
    AtomFunArrow -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom) loc

openModule :: forall r. Members '[InfoTableBuilder, JudocStash, NameIdGen] r => ParsecS r (OpenModule 'Parsed)
openModule = do
  kw kwOpen
  _openModuleImport <- isJust <$> optional (kw kwImport)
  _openModuleName <- name
  _openParameters <- many atomicExpression
  _openUsingHiding <- optional usingOrHiding
  _openPublic <- maybe NoPublic (const Public) <$> optional (kw kwPublic)
  return OpenModule {..}
  where
    usingOrHiding :: ParsecS r UsingHiding
    usingOrHiding =
      (kw kwUsing >> (Using <$> symbolList))
        <|> (kw kwHiding >> (Hiding <$> symbolList))
