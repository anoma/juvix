module Juvix.Compiler.Concrete.Translation.FromSource
  ( module Juvix.Compiler.Concrete.Translation.FromSource,
    module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Compiler.Concrete.Data.ParsedInfoTable,
    module Juvix.Parser.Error,
  )
where

import Data.ByteString.UTF8 qualified as BS
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Data.Yaml
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
import Juvix.Compiler.Concrete.Extra (MonadParsec (takeWhile1P))
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer hiding
  ( symbol,
  )
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Error
import Juvix.Prelude
import Juvix.Prelude.Pretty
  ( Pretty,
    prettyText,
  )

type JudocStash = State (Maybe (Judoc 'Parsed))

type PragmasStash = State (Maybe ParsedPragmas)

fromSource ::
  (Members '[PathResolver, Files, Error JuvixError, NameIdGen] r) =>
  EntryPoint ->
  Sem r ParserResult
fromSource e = mapError (JuvixError @ParserError) $ do
  (_resultBuilderState, _resultTable, _resultModules) <- runParserInfoTableBuilder (runReader e getParsedModuleTops)
  let _resultEntry = e
  return ParserResult {..}
  where
    getParsedModuleTops ::
      forall r.
      (Members '[PathResolver, Files, Error ParserError, InfoTableBuilder, NameIdGen] r) =>
      Sem r (NonEmpty (Module 'Parsed 'ModuleTop))
    getParsedModuleTops = case (e ^. entryPointStdin, e ^. entryPointModulePaths) of
      (Nothing, []) -> throw $ ErrStdinOrFile StdinOrFileError
      (Just txt, _) ->
        runModuleStdinParser txt >>= \case
          Left err -> throw err
          Right r -> return (r :| [])
      (_, x : xs) -> mapM goFile (x :| xs)

    goFile ::
      forall r.
      (Members '[PathResolver, Files, Error ParserError, InfoTableBuilder, NameIdGen] r) =>
      Path Abs File ->
      Sem r (Module 'Parsed 'ModuleTop)
    goFile fileName = do
      input <- getFileContents fileName
      mp <- runModuleParser fileName input
      case mp of
        Left er -> throw er
        Right m -> return m
      where
        getFileContents :: Path Abs File -> Sem r Text
        getFileContents fp
          | Just fp == e ^? mainModulePath,
            Just txt <- e ^. entryPointStdin =
              return txt
          | otherwise = readFile' fp

data ReplInput
  = ReplExpression (ExpressionAtoms 'Parsed)
  | ReplImport (Import 'Parsed)
  | ReplOpenImport (OpenModule 'Parsed)

expressionFromTextSource ::
  Members '[Error JuvixError, NameIdGen] r =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionFromTextSource fp txt = mapError (JuvixError @ParserError) $ do
  exp <- runExpressionParser fp txt
  case exp of
    Left e -> throw e
    Right exp' -> return exp'

replInputFromTextSource ::
  Members '[Error JuvixError, NameIdGen, Files, PathResolver, InfoTableBuilder] r =>
  Path Abs File ->
  Text ->
  Sem r ReplInput
replInputFromTextSource fp txt = mapError (JuvixError @ParserError) $ runReplInputParser fp txt

runReplInputParser ::
  Members '[Files, NameIdGen, Error ParserError, PathResolver, InfoTableBuilder] r =>
  Path Abs File ->
  Text ->
  Sem r ReplInput
runReplInputParser fileName input = do
  m <-
    evalState (Nothing @ParsedPragmas) $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT replInput (toFilePath fileName) input
  case m of
    Left err -> throw (ErrMegaparsec (MegaparsecError err))
    Right r -> return r

runModuleParser :: Members '[Error ParserError, Files, PathResolver, NameIdGen, InfoTableBuilder] r => Path Abs File -> Text -> Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleParser fileName input = do
  m <-
    evalState (Nothing @ParsedPragmas) $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT topModuleDef (toFilePath fileName) input
  case m of
    Left err -> return (Left (ErrMegaparsec (MegaparsecError err)))
    Right r -> registerModule r $> Right r

runModuleStdinParser ::
  Members '[Error ParserError, Files, PathResolver, NameIdGen, InfoTableBuilder] r =>
  Text ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleStdinParser input = do
  m <-
    evalState (Nothing @ParsedPragmas) $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT topModuleDefStdin (toFilePath formatStdinPath) input
  case m of
    Left err -> return (Left (ErrMegaparsec (MegaparsecError err)))
    Right r -> registerModule r $> Right r

runExpressionParser ::
  Members '[NameIdGen] r =>
  Path Abs File ->
  Text ->
  Sem r (Either ParserError (ExpressionAtoms 'Parsed))
runExpressionParser fileName input = do
  m <-
    runParserInfoTableBuilder $
      evalState (Nothing @ParsedPragmas) $
        evalState (Nothing @(Judoc 'Parsed)) $
          P.runParserT parseExpressionAtoms (toFilePath fileName) input
  case m of
    (_, _, Left err) -> return (Left (ErrMegaparsec (MegaparsecError err)))
    (_, _, Right r) -> return (Right r)

-- | The first pipe is optional, and thus we need a `Maybe`. The rest of the elements are guaranted to be given a `Just`.
pipeSep1 :: Member InfoTableBuilder r => (Irrelevant (Maybe KeywordRef) -> ParsecS r a) -> ParsecS r (NonEmpty a)
pipeSep1 e = do
  p <- Irrelevant <$> optional (kw kwPipe)
  h <- e p
  (h :|) <$> many (kw kwPipe >>= e . Irrelevant . Just)

top ::
  (Member InfoTableBuilder r) =>
  ParsecS r a ->
  ParsecS r a
top p = space >> p <* (optional semicolon >> P.eof)

topModuleDefStdin ::
  (Members '[Error ParserError, Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDefStdin = do
  void (optional stashJudoc)
  top moduleDef

topModuleDef ::
  (Members '[Error ParserError, Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = do
  void (optional stashJudoc)
  void (optional stashPragmas)
  m <- top moduleDef
  P.lift (checkPath (m ^. modulePath))
  return m
  where
    checkPath :: Members '[PathResolver, Error ParserError] s => TopModulePath -> Sem s ()
    checkPath path = do
      let actualPath :: Path Abs File = getLoc path ^. intervalFile
      mexpectedPath <- expectedModulePath actualPath path
      whenJust mexpectedPath $ \expectedPath ->
        unlessM (equalPaths expectedPath actualPath) $
          throw
            ( ErrWrongTopModuleName
                WrongTopModuleName
                  { _wrongTopModuleNameActualName = path,
                    _wrongTopModuleNameExpectedPath = expectedPath,
                    _wrongTopModuleNameActualPath = actualPath
                  }
            )

replInput :: forall r. Members '[Files, PathResolver, InfoTableBuilder, JudocStash, NameIdGen, Error ParserError, State (Maybe ParsedPragmas)] r => ParsecS r ReplInput
replInput =
  P.label "<repl input>" $
    (ReplExpression <$> parseExpressionAtoms)
      <|> (ReplImport <$> import_)
      <|> (ReplOpenImport <$> openModule)

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Symbol
symbol = uncurry (flip WithLoc) <$> identifierL

dottedSymbol :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry (flip WithLoc)) <$> dottedIdentifier

name :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (SymbolPath p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol semicolon)

topModulePath :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: (Members '[Files, Error ParserError, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Statement 'Parsed)
statement = P.label "<top level statement>" $ do
  void (optional stashJudoc)
  void (optional stashPragmas)
  (StatementOperator <$> operatorSyntaxDef)
    <|> (StatementOpenModule <$> openModule)
    <|> (StatementImport <$> import_)
    <|> (StatementInductive <$> inductiveDef Nothing)
    <|> (StatementModule <$> moduleDef)
    <|> (StatementAxiom <$> axiomDef Nothing)
    <|> builtinStatement
    <|> ( either StatementTypeSignature StatementFunctionClause
            <$> auxTypeSigFunClause
        )

stashPragmas :: forall r. (Members '[InfoTableBuilder, PragmasStash, NameIdGen] r) => ParsecS r ()
stashPragmas = do
  pragmas <- withLoc parsePragmas
  P.lift (registerPragmas (getLoc pragmas))
  P.lift (put (Just pragmas))
  where
    parsePragmas :: ParsecS r (WithSource Pragmas)
    parsePragmas = do
      void (P.chunk Str.pragmasStart)
      off <- P.getOffset
      str <- P.manyTill P.anySingle (P.chunk Str.pragmasEnd)
      space
      let bs = BS.fromString str
      case decodeEither' bs of
        Left err -> parseFailure off (prettyPrintParseException err)
        Right pragmas -> return $ WithSource (fromString str) pragmas

stashJudoc :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r ()
stashJudoc = do
  b <- judocBlocks
  many judocEmptyLine
  P.lift (modify (<> Just b))
  where
    judocBlocks :: ParsecS r (Judoc 'Parsed)
    judocBlocks = Judoc <$> some judocBlock

    judocBlock :: ParsecS r (JudocBlock 'Parsed)
    judocBlock = do
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
      _exampleId <- P.lift freshNameId
      (_exampleExpression, _exampleLoc) <- interval parseExpressionAtoms
      semicolon
      space
      return (JudocExample Example {..})

    judocLine :: ParsecS r (JudocParagraphLine 'Parsed)
    judocLine = lexeme $ do
      P.try (judocStart >> P.notFollowedBy (P.choice [judocExampleStart, void P.newline]))
      ln <- JudocParagraphLine <$> some1 (withLoc judocAtom)
      P.newline
      return ln

judocAtom :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (JudocAtom 'Parsed)
judocAtom =
  JudocText <$> judocAtomText
    <|> JudocExpression <$> judocExpression
  where
    judocAtomText :: ParsecS r Text
    judocAtomText = judocText (takeWhile1P Nothing isValidText)
      where
        isValidText :: Char -> Bool
        isValidText = (`notElem` ['\n', ';'])

    judocExpression :: ParsecS r (ExpressionAtoms 'Parsed)
    judocExpression = do
      judocText_ (P.char ';')
      e <- parseExpressionAtoms
      judocText_ (P.char ';')
      return e

builtinInductive :: Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r => ParsecS r (WithLoc BuiltinInductive)
builtinInductive = builtinHelper

builtinFunction :: Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r => ParsecS r (WithLoc BuiltinFunction)
builtinFunction = builtinHelper

builtinAxiom :: Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r => ParsecS r (WithLoc BuiltinAxiom)
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r (WithLoc a)
builtinHelper =
  P.choice
    [ (`WithLoc` a) <$> onlyInterval (kw (asciiKw (prettyText a)))
      | a <- allElements
    ]

builtinInductiveDef :: Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r => WithLoc BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r =>
  WithLoc BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinTypeSig ::
  Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r =>
  WithLoc BuiltinFunction ->
  ParsecS r (TypeSignature 'Parsed)
builtinTypeSig b = do
  terminating <- optional (kw kwTerminating)
  fun <- symbol
  typeSignature terminating fun (Just b)

builtinStatement :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Statement 'Parsed)
builtinStatement = do
  void (kw kwBuiltin)
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementTypeSignature . builtinTypeSig)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Precedence
precedence = PrecNat <$> (fst <$> decimal)

operatorSyntaxDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r OperatorSyntaxDef
operatorSyntaxDef = do
  (_fixityArity, _opKw) <- arity
  _fixityPrecedence <- precedence
  _opSymbol <- symbol
  let _opFixity = Fixity {..}
  return OperatorSyntaxDef {..}
  where
    arity :: ParsecS r (OperatorArity, KeywordRef)
    arity =
      (Binary AssocRight,) <$> kw kwInfixr
        <|> (Binary AssocLeft,) <$> kw kwInfixl
        <|> (Binary AssocNone,) <$> kw kwInfix
        <|> (Unary AssocPostfix,) <$> kw kwPostfix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: forall r. Members '[Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen, Error ParserError] r => ParsecS r (Import 'Parsed)
import_ = do
  _importKw <- kw kwImport
  _importModule <- topModulePath
  P.lift (importedModule _importModule)
  _importAsName <- optional pasName
  return Import {..}
  where
    pasName :: ParsecS r TopModulePath
    pasName = void (kw kwAs) >> topModulePath

withPath' ::
  forall r a.
  (Members '[PathResolver, Error ParserError] r) =>
  TopModulePath ->
  (Path Abs File -> Sem r a) ->
  Sem r a
withPath' mp a = withPathFile mp (either err a)
  where
    err :: PathResolverError -> Sem r a
    err = throw . ErrTopModulePath . TopModulePathError mp

importedModule :: forall r. Members '[PathResolver, InfoTableBuilder, NameIdGen, Files, Error ParserError] r => TopModulePath -> Sem r ()
importedModule t = unlessM (moduleVisited t) go
  where
    go :: Sem r ()
    go = withPath' t $ \path -> do
      visitModule t
      txt <- readFile' path
      eitherM throw (const (return ())) (runModuleParser path txt)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionAtom :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  P.label "<expression>" $
    AtomLiteral <$> P.try literal
      <|> (AtomIdentifier <$> name)
      <|> (AtomUniverse <$> universe)
      <|> (AtomLambda <$> lambda)
      <|> (AtomCase <$> case_)
      <|> (AtomFunction <$> function)
      <|> (AtomLetBlock <$> letBlock)
      <|> (AtomFunArrow <$> kw kwRightArrow)
      <|> (AtomHole <$> hole)
      <|> parens (AtomParens <$> parseExpressionAtoms)
      <|> braces (AtomBraces <$> withLoc parseExpressionAtoms)

parseExpressionAtoms ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- interval (P.some expressionAtom)
  return ExpressionAtoms {..}

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (HoleType 'Parsed)
hole = snd <$> interval (kw kwHole)

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r LiteralLoc
literalInteger = do
  (x, loc) <- integer
  return (WithLoc loc (LitInteger x))

literalString :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (WithLoc loc (LitString x))

literal :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

letClause :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (LetClause 'Parsed)
letClause = either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (LetBlock 'Parsed)
letBlock = do
  _letKw <- kw kwLet
  _letClauses <- P.sepEndBy1 letClause semicolon
  kw kwIn
  _letExpression <- parseExpressionAtoms
  return LetBlock {..}

caseBranch :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (CaseBranch 'Parsed)
caseBranch = do
  _caseBranchPipe <- kw kwPipe
  _caseBranchPattern <- parsePatternAtoms
  kw kwAssign
  _caseBranchExpression <- parseExpressionAtoms
  return CaseBranch {..}

case_ :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Case 'Parsed)
case_ = do
  _caseKw <- kw kwCase
  _caseExpression <- parseExpressionAtoms
  _caseBranches <- some1 caseBranch
  let _caseParens = False
  return Case {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Universe
universe = do
  i <- snd <$> interval (kw kwType)
  uni <- optional decimal
  return
    ( case uni of
        Nothing -> Universe Nothing i
        Just (lvl, i') -> Universe (Just lvl) (i <> i')
    )

getJudoc :: (Member JudocStash r) => ParsecS r (Maybe (Judoc 'Parsed))
getJudoc = P.lift $ do
  j <- get
  put (Nothing @(Judoc 'Parsed))
  return j

getPragmas :: (Member PragmasStash r) => ParsecS r (Maybe ParsedPragmas)
getPragmas = P.lift $ do
  j <- get
  put (Nothing @ParsedPragmas)
  return j

typeSignature ::
  Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r =>
  Maybe KeywordRef ->
  Symbol ->
  Maybe (WithLoc BuiltinFunction) ->
  ParsecS r (TypeSignature 'Parsed)
typeSignature _sigTerminating _sigName _sigBuiltin = do
  kw kwColon
  _sigType <- parseExpressionAtoms
  _sigDoc <- getJudoc
  _sigPragmas <- getPragmas
  _sigBody <- optional (kw kwAssign >> parseExpressionAtoms)
  return TypeSignature {..}

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  terminating <- optional (kw kwTerminating)
  sym <- symbol
  (Left <$> typeSignature terminating sym Nothing)
    <|> (Right <$> functionClause sym)

axiomDef ::
  Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r =>
  Maybe (WithLoc BuiltinAxiom) ->
  ParsecS r (AxiomDef 'Parsed)
axiomDef _axiomBuiltin = do
  _axiomKw <- kw kwAxiom
  _axiomDoc <- getJudoc
  _axiomPragmas <- getPragmas
  _axiomName <- symbol
  kw kwColon
  _axiomType <- parseExpressionAtoms
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

implicitOpen :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r IsImplicit
implicitOpen =
  lbrace $> Implicit
    <|> lparen $> Explicit

implicitClose :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => IsImplicit -> ParsecS r ()
implicitClose = \case
  Implicit -> rbrace
  Explicit -> rparen

functionParams :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (FunctionParameters 'Parsed)
functionParams = do
  (_paramNames, _paramImplicit) <- P.try $ do
    impl <- implicitOpen
    n <- some pName
    kw kwColon
    return (n, impl)
  _paramType <- parseExpressionAtoms
  implicitClose _paramImplicit
  return FunctionParameters {..}
  where
    pName :: ParsecS r (Maybe Symbol)
    pName =
      (Just <$> symbol)
        <|> (Nothing <$ kw kwWildcard)

function :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Function 'Parsed)
function = do
  _funParameters <- functionParams
  _funKw <- kw kwRightArrow
  _funReturn <- parseExpressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (LambdaClause 'Parsed)
lambdaClause _lambdaPipe = do
  _lambdaParameters <- P.some patternAtom
  kw kwAssign
  _lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Lambda 'Parsed)
lambda = do
  _lambdaKw <- kw kwLambda
  _lambdaClauses <- braces (pipeSep1 lambdaClause)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r => Maybe (WithLoc BuiltinInductive) -> ParsecS r (InductiveDef 'Parsed)
inductiveDef _inductiveBuiltin = do
  _inductivePositive <- optional (kw kwPositive)
  _inductiveKw <- kw kwInductive
  _inductiveDoc <- getJudoc
  _inductivePragmas <- getPragmas
  _inductiveName <- symbol P.<?> "<type name>"
  _inductiveParameters <-
    P.many inductiveParams
      P.<?> "<type parameter e.g. '(A : Type)'>"
  _inductiveType <-
    optional (kw kwColon >> parseExpressionAtoms)
      P.<?> "<type annotation e.g. ': Type'>"
  kw kwAssign P.<?> "<assignment symbol ':='>"
  _inductiveConstructors <-
    pipeSep1 constructorDef
      P.<?> "<constructor definition>"
  return InductiveDef {..}

inductiveParams :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParams = parens $ do
  _inductiveParametersNames <- some1 symbol
  kw kwColon
  _inductiveParametersType <- parseExpressionAtoms
  return InductiveParameters {..}

constructorDef :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (InductiveConstructorDef 'Parsed)
constructorDef _constructorPipe = do
  _constructorDoc <- optional stashJudoc >> getJudoc
  _constructorPragmas <- optional stashPragmas >> getPragmas
  _constructorName <- symbol P.<?> "<constructor name>"
  _constructorType <-
    kw kwColon >> parseExpressionAtoms
      P.<?> "<constructor type signature (:)>"
  return InductiveConstructorDef {..}

wildcard :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval (kw kwWildcard)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtomAnon :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomAnon =
  PatternAtomWildcard <$> wildcard
    <|> PatternAtomParens <$> parens parsePatternAtomsNested
    <|> PatternAtomBraces <$> braces parsePatternAtomsNested

patternAtomAt :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => SymbolType 'Parsed -> ParsecS r (PatternAtom 'Parsed)
patternAtomAt s = kw kwAt >> PatternAtomAt . PatternBinding s <$> patternAtom

patternAtomNamed :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtomNamed nested = do
  off <- P.getOffset
  n <- name
  case n of
    NameQualified _ -> return (PatternAtomIden n)
    NameUnqualified s -> do
      checkWrongEq off s
      patternAtomAt s <|> return (PatternAtomIden n)
  where
    checkWrongEq :: Int -> WithLoc Text -> ParsecS r ()
    checkWrongEq off t =
      when
        (not nested && t ^. withLocParam == "=")
        (parseFailure off "expected \":=\" instead of \"=\"")

patternAtomNested :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomNested = patternAtom' True

patternAtom :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtom 'Parsed)
patternAtom = patternAtom' False

patternAtom' :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtom' nested = P.label "<pattern>" $ patternAtomNamed nested <|> patternAtomAnon

parsePatternAtoms :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- interval (P.some patternAtom)
  return PatternAtoms {..}

parsePatternAtomsNested :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtomsNested = do
  (_patternAtoms, _patternAtomsLoc) <- interval (P.some patternAtomNested)
  return PatternAtoms {..}

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Symbol -> ParsecS r (FunctionClause 'Parsed)
functionClause _clauseOwnerFunction = do
  _clausePatterns <- P.many patternAtom
  kw kwAssign
  _clauseBody <- parseExpressionAtoms
  return FunctionClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: forall t r. (SingI t, Members '[Error ParserError, Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Module 'Parsed t)
moduleDef = P.label "<module definition>" $ do
  _moduleKw <- kw kwModule
  _moduleDoc <- getJudoc
  _modulePragmas <- getPragmas
  _modulePath <- pmodulePath
  _moduleParameters <- many inductiveParams
  semicolon
  _moduleBody <- P.sepEndBy statement semicolon
  _moduleKwEnd <- endModule
  return Module {..}
  where
    endModule :: ParsecS r (ModuleEndType t)
    endModule = case sing :: SModuleIsTop t of
      SModuleLocal -> kw kwEnd
      SModuleTop -> void (optional (kw kwEnd >> semicolon))

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ExpressionAtoms 'Parsed)
atomicExpression = do
  (atom, loc) <- interval expressionAtom
  case atom of
    AtomFunArrow {} -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom) loc

openModule :: forall r. (Members '[Error ParserError, PathResolver, Files, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (OpenModule 'Parsed)
openModule = do
  _openModuleKw <- kw kwOpen
  _openModuleImportKw <- optional (kw kwImport)
  _openModuleName <- name
  whenJust _openModuleImportKw (const (P.lift (importedModule (moduleNameToTopModulePath _openModuleName))))
  _openParameters <- many atomicExpression
  _openUsingHiding <- optional usingOrHiding
  _openPublic <- maybe NoPublic (const Public) <$> optional (kw kwPublic)
  return OpenModule {..}
  where
    usingOrHiding :: ParsecS r UsingHiding
    usingOrHiding =
      (kw kwUsing >> (Using <$> symbolList))
        <|> (kw kwHiding >> (Hiding <$> symbolList))
