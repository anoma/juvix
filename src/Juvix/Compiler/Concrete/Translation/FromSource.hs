module Juvix.Compiler.Concrete.Translation.FromSource
  ( module Juvix.Compiler.Concrete.Translation.FromSource,
    module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Compiler.Concrete.Data.ParsedInfoTable,
    module Juvix.Parser.Error,
  )
where

import Control.Applicative.Permutations
import Data.ByteString.UTF8 qualified as BS
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Data.Text qualified as Text
import Juvix.Compiler.Concrete.Data.Highlight.Input (HighlightBuilder, ignoreHighlightBuilder)
import Juvix.Compiler.Concrete.Data.ParsedInfoTable
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder
import Juvix.Compiler.Concrete.Extra (takeWhile1P)
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer hiding
  ( symbol,
  )
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.Yaml
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
  (Members '[HighlightBuilder, PathResolver, Files, Error JuvixError, NameIdGen] r) =>
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
      (Just txt, x : _) ->
        runModuleParser x txt >>= \case
          Left err -> throw err
          Right r -> return (r :| [])
      (Just txt, []) ->
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
  (Members '[Error JuvixError, NameIdGen] r) =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionFromTextSource fp txt = mapError (JuvixError @ParserError) $ do
  exp <- runExpressionParser fp txt
  case exp of
    Left e -> throw e
    Right exp' -> return exp'

replInputFromTextSource ::
  (Members '[Error JuvixError, NameIdGen, Files, PathResolver, InfoTableBuilder] r) =>
  Path Abs File ->
  Text ->
  Sem r ReplInput
replInputFromTextSource fp txt = mapError (JuvixError @ParserError) $ runReplInputParser fp txt

runReplInputParser ::
  (Members '[Files, NameIdGen, Error ParserError, PathResolver, InfoTableBuilder] r) =>
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

runModuleParser :: (Members '[Error ParserError, Files, PathResolver, NameIdGen, InfoTableBuilder] r) => Path Abs File -> Text -> Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleParser fileName input = do
  m <-
    evalState (Nothing @ParsedPragmas)
      . evalState (Nothing @(Judoc 'Parsed))
      $ P.runParserT topModuleDef (toFilePath fileName) input
  case m of
    Left err -> return (Left (ErrMegaparsec (MegaparsecError err)))
    Right r -> registerModule r $> Right r

runModuleStdinParser ::
  (Members '[Error ParserError, Files, PathResolver, NameIdGen, InfoTableBuilder] r) =>
  Text ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleStdinParser input = do
  m <-
    evalState (Nothing @ParsedPragmas)
      . evalState (Nothing @(Judoc 'Parsed))
      $ P.runParserT topModuleDefStdin (toFilePath formatStdinPath) input
  case m of
    Left err -> return (Left (ErrMegaparsec (MegaparsecError err)))
    Right r -> registerModule r $> Right r

runExpressionParser ::
  (Members '[NameIdGen] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either ParserError (ExpressionAtoms 'Parsed))
runExpressionParser fileName input = do
  m <-
    ignoreHighlightBuilder
      . runParserInfoTableBuilder
      . evalState (Nothing @ParsedPragmas)
      . evalState (Nothing @(Judoc 'Parsed))
      $ P.runParserT parseExpressionAtoms (toFilePath fileName) input
  case m of
    (_, _, Left err) -> return (Left (ErrMegaparsec (MegaparsecError err)))
    (_, _, Right r) -> return (Right r)

-- | The first pipe is optional, and thus we need a `Maybe`. The rest of the elements are guaranted to be given a `Just`.
pipeSep1 :: (Member InfoTableBuilder r) => (Irrelevant (Maybe KeywordRef) -> ParsecS r a) -> ParsecS r (NonEmpty a)
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
  optional_ stashJudoc
  top moduleDef

topModuleDef ::
  (Members '[Error ParserError, Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = do
  optional_ stashJudoc
  optional_ stashPragmas
  m <- top moduleDef
  P.lift (checkPath (m ^. modulePath))
  return m
  where
    checkPath :: (Members '[PathResolver, Error ParserError] s) => TopModulePath -> Sem s ()
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

replInput :: forall r. (Members '[Files, PathResolver, InfoTableBuilder, JudocStash, NameIdGen, Error ParserError, State (Maybe ParsedPragmas)] r) => ParsecS r ReplInput
replInput =
  P.label "<repl input>" $
    ReplExpression
      <$> parseExpressionAtoms
      <|> either ReplImport ReplOpenImport
        <$> importOpenSyntax

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

usingItem :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (UsingItem 'Parsed)
usingItem = do
  _usingModuleKw <- optional (kw kwModule)
  _usingSymbol <- symbol
  alias <- optional $ do
    k <- Irrelevant <$> kw kwAs
    (k,) <$> symbol
  let _usingAsKw = mapM fst alias
      _usingAs = snd <$> alias
  return UsingItem {..}

hidingItem :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (HidingItem 'Parsed)
hidingItem = do
  _hidingModuleKw <- optional (kw kwModule)
  _hidingSymbol <- symbol
  return HidingItem {..}

phidingList :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (HidingList 'Parsed)
phidingList = do
  _hidingKw <- Irrelevant <$> kw kwHiding
  l <- kw delimBraceL
  _hidingList <- P.sepBy1 hidingItem semicolon
  r <- kw delimBraceR
  return
    HidingList
      { _hidingBraces = Irrelevant (l, r),
        ..
      }

pusingList :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (UsingList 'Parsed)
pusingList = do
  _usingKw <- Irrelevant <$> kw kwUsing
  l <- kw delimBraceL
  _usingList <- P.sepBy1 usingItem semicolon
  r <- kw delimBraceR
  return
    UsingList
      { _usingBraces = Irrelevant (l, r),
        ..
      }

topModulePath :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

infixl 3 <?|>

-- | Tries the left alternative. If it fails, backtracks and restores the contents of the pragmas and judoc stashes. Then parses the right alternative
(<?|>) :: (Members '[PragmasStash, JudocStash] r) => ParsecS r a -> ParsecS r a -> ParsecS r a
l <?|> r = do
  p <- P.lift (get @(Maybe ParsedPragmas))
  j <- P.lift (get @(Maybe (Judoc 'Parsed)))
  let recover = do
        P.lift (put p)
        P.lift (put j)
        r
  P.withRecovery (const recover) (P.try l)

statement :: (Members '[Files, Error ParserError, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Statement 'Parsed)
statement = P.label "<top level statement>" $ do
  optional_ stashJudoc
  optional_ stashPragmas
  ms <-
    optional
      ( either StatementImport StatementOpenModule
          <$> importOpenSyntax
          <|> StatementOpenModule
            <$> openModule
          <|> StatementSyntax
            <$> syntaxDef
          <|> StatementInductive
            <$> inductiveDef Nothing
          <|> StatementModule
            <$> moduleDef
          <|> StatementAxiom
            <$> axiomDef Nothing
          <|> builtinStatement
          <|> StatementFunctionDef
            <$> functionDefinition Nothing
      )
  case ms of
    Just s -> return s
    Nothing -> do
      mj <- peekJudoc
      case mj of
        Nothing -> P.failure Nothing mempty
        Just j -> P.lift . throw . ErrDanglingJudoc . DanglingJudoc $ j

stashPragmas :: forall r. (Members '[InfoTableBuilder, PragmasStash, NameIdGen] r) => ParsecS r ()
stashPragmas = do
  pragmas <- withLoc parsePragmas
  P.lift (registerPragmas (getLoc pragmas))
  P.lift (put (Just pragmas))
  where
    parsePragmas :: ParsecS r (WithSource Pragmas)
    parsePragmas = parseYaml Str.pragmasStart Str.pragmasEnd

parseYaml :: (Member InfoTableBuilder r, FromJSON a) => Text -> Text -> ParsecS r (WithSource a)
parseYaml l r = do
  void (P.chunk l)
  off <- P.getOffset
  str <- P.manyTill P.anySingle (P.chunk r)
  let str' =
        if
            | elem '\n' str -> str
            | otherwise -> '{' : str ++ "}"
  space
  let bs = BS.fromString str'
  case decodeEither bs of
    Left err -> parseFailure off (prettyPrintParseException err)
    Right yaml -> return $ WithSource (fromString str) yaml

stashJudoc :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r ()
stashJudoc = do
  b <- judoc
  many (judocEmptyLine False)
  P.lift (modify (<> Just b))
  where
    judoc :: ParsecS r (Judoc 'Parsed)
    judoc = Judoc <$> some1 judocGroup

    judocGroup :: ParsecS r (JudocGroup 'Parsed)
    judocGroup =
      JudocGroupBlock
        <$> judocParagraphBlock
        <|> JudocGroupLines
          <$> some1 (judocBlock False)

    judocEmptyLine :: Bool -> ParsecS r ()
    judocEmptyLine inBlock =
      lexeme
        . void
        $ if
            | inBlock -> P.newline
            | otherwise -> P.try (judocStart >> P.newline)

    judocBlock :: Bool -> ParsecS r (JudocBlock 'Parsed)
    judocBlock inBlock = do
      p <-
        judocExample inBlock
          <|> judocParagraphLines inBlock
      void (many (judocEmptyLine inBlock))
      return p

    judocParagraphBlock :: ParsecS r (JudocBlockParagraph 'Parsed)
    judocParagraphBlock = do
      _judocBlockParagraphStart <- judocBlockStart
      whiteSpace
      _judocBlockParagraphBlocks <- many (judocBlock True)
      _judocBlockParagraphEnd <- judocBlockEnd
      return
        JudocBlockParagraph {..}

    judocParagraphLines :: Bool -> ParsecS r (JudocBlock 'Parsed)
    judocParagraphLines inBlock = JudocLines <$> some1 (paragraphLine inBlock)

    judocExample :: Bool -> ParsecS r (JudocBlock 'Parsed)
    judocExample inBlock = do
      if
          | inBlock -> judocExampleStart
          | otherwise -> P.try (judocStart >> judocExampleStart)
      _exampleId <- P.lift freshNameId
      (_exampleExpression, _exampleLoc) <- interval parseExpressionAtoms
      semicolon
      space
      return (JudocExample Example {..})

    paragraphLine :: Bool -> ParsecS r (JudocLine 'Parsed)
    paragraphLine inBlock = do
      kwstart <-
        if
            | inBlock -> return Nothing
            | otherwise -> P.try $ do
                s <- judocStart
                P.notFollowedBy (P.choice [judocExampleStart, void P.newline])
                return (Just s)
      l <- JudocLine kwstart . trimEnds <$> some1 (withLoc (judocAtom inBlock))
      if
          | inBlock -> optional_ P.newline
          | otherwise -> void P.newline >> space
      return l
      where
        trimEnds :: NonEmpty (WithLoc (JudocAtom 'Parsed)) -> NonEmpty (WithLoc (JudocAtom 'Parsed))
        trimEnds = over (_last1 . withLocParam) (tr Text.stripEnd)
          where
            tr :: (Text -> Text) -> JudocAtom 'Parsed -> JudocAtom 'Parsed
            tr strp a = case a of
              JudocExpression {} -> a
              JudocText txt -> JudocText (strp txt)

judocAtom ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  Bool ->
  ParsecS r (JudocAtom 'Parsed)
judocAtom inBlock =
  JudocText
    <$> judocAtomText
    <|> JudocExpression
      <$> judocExpression
  where
    judocAtomText :: ParsecS r Text
    judocAtomText =
      judocText $
        if
            | inBlock -> goBlockAtom
            | otherwise -> takeWhile1P Nothing isValidText
      where
        -- We prefer to use takeWhileP when possible since it is more efficient than some
        goBlockAtom :: ParsecS r Text
        goBlockAtom = do
          txt <- P.takeWhileP Nothing (`notElem` stopChars)
          dashes <- many (P.notFollowedBy judocBlockEnd >> P.single '-')
          rest <-
            if
                | not (Text.null txt) || notNull dashes -> optional goBlockAtom
                | otherwise -> mzero
          return (txt <> pack dashes <> fromMaybe mempty rest)
          where
            stopChars :: [Char]
            stopChars = '-' : invalidChars

        invalidChars :: [Char]
        invalidChars = ['\n', ';']

        isValidText :: Char -> Bool
        isValidText = (`notElem` invalidChars)

    judocExpression :: ParsecS r (ExpressionAtoms 'Parsed)
    judocExpression = do
      judocText_ (P.char ';')
      e <- parseExpressionAtoms
      judocText_ (P.char ';')
      return e

builtinInductive :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (WithLoc BuiltinInductive)
builtinInductive = builtinHelper

builtinFunction :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (WithLoc BuiltinFunction)
builtinFunction = builtinHelper

builtinAxiom :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (WithLoc BuiltinAxiom)
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r (WithLoc a)
builtinHelper =
  P.choice
    [ (`WithLoc` a) <$> onlyInterval (kw (asciiKw (prettyText a)))
      | a <- allElements
    ]

builtinInductiveDef :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => WithLoc BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  WithLoc BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinFunctionDef ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  WithLoc BuiltinFunction ->
  ParsecS r (FunctionDef 'Parsed)
builtinFunctionDef = functionDefinition . Just

builtinStatement :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Statement 'Parsed)
builtinStatement = do
  void (kw kwBuiltin)
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementFunctionDef . builtinFunctionDef)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

--------------------------------------------------------------------------------
-- Syntax declaration
--------------------------------------------------------------------------------

syntaxDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (SyntaxDef 'Parsed)
syntaxDef = do
  syn <- kw kwSyntax
  SyntaxFixity
    <$> fixitySyntaxDef syn
      <?|> SyntaxFixityNew -- TODO replace <?|> after removing old syntax
    <$> fixitySyntaxDefNew syn
    <|> SyntaxOperator
      <$> operatorSyntaxDef syn
    <|> SyntaxIterator
      <$> iteratorSyntaxDef syn
    <|> SyntaxAlias
      <$> aliasDef syn

aliasDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => KeywordRef -> ParsecS r (AliasDef 'Parsed)
aliasDef synKw = do
  let _aliasDefSyntaxKw = Irrelevant synKw
  _aliasDefAliasKw <- Irrelevant <$> kw kwAlias
  _aliasDefName <- symbol
  kw kwAssign
  _aliasDefAsName <- name
  return AliasDef {..}

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Precedence
precedence = PrecNat <$> (fst <$> decimal)

parsedFixityInfoNew :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ParsedFixityInfoNew 'Parsed)
parsedFixityInfoNew = do
  _nfixityArity <- withLoc ari
  l <- kw delimBraceL
  (_nfixityAssoc, _nfixityPrecBelow, _nfixityPrecAbove, _nfixityPrecSame) <- intercalateEffect semicolon $ do
    as <- toPermutationWithDefault Nothing (Just <$> assoc)
    bel <- toPermutationWithDefault Nothing (Just <$> belowAbove kwBelow)
    abov <- toPermutationWithDefault Nothing (Just <$> belowAbove kwAbove)
    sam <- toPermutationWithDefault Nothing (Just <$> same)
    pure (as, bel, abov, sam)
  r <- kw delimBraceR
  let _nfixityBraces = Irrelevant (l, r)
  return ParsedFixityInfoNew {..}
  where
    same :: ParsecS r Symbol
    same = do
      kw kwSame
      kw kwAssign
      symbol

    belowAbove :: Keyword -> ParsecS r [Symbol]
    belowAbove aboveOrBelow = do
      kw aboveOrBelow
      kw kwAssign
      kw kwBracketL
      r <- P.sepEndBy symbol semicolon
      kw kwBracketR
      return r

    assoc :: ParsecS r BinaryAssoc
    assoc = do
      void (kw kwAssoc >> kw kwAssign)
      kw kwLeft $> AssocLeft
        <|> kw kwRight $> AssocRight
        <|> kw kwNone $> AssocNone

    ari :: ParsecS r Arity
    ari =
      kw kwUnary
        $> Unary
        <|> kw kwBinary
          $> Binary

fixitySyntaxDefNew :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => KeywordRef -> ParsecS r (FixitySyntaxDefNew 'Parsed)
fixitySyntaxDefNew _nfixitySyntaxKw = P.label "<fixity declaration>" $ do
  _nfixityDoc <- getJudoc
  _nfixityKw <- kw kwFixity
  _nfixitySymbol <- symbol
  _nfixityAssignKw <- kw kwAssign
  _nfixityInfo <- parsedFixityInfoNew
  return FixitySyntaxDefNew {..}

fixitySyntaxDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => KeywordRef -> ParsecS r (FixitySyntaxDef 'Parsed)
fixitySyntaxDef _fixitySyntaxKw = P.label "<fixity declaration>" $ do
  _fixityKw <- kw kwFixity
  _fixitySymbol <- symbol
  _fixityInfo <- withLoc (parseYaml "{" "}")
  _fixityDoc <- getJudoc
  return FixitySyntaxDef {..}

operatorSyntaxDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => KeywordRef -> ParsecS r OperatorSyntaxDef
operatorSyntaxDef _opSyntaxKw = do
  _opKw <- kw kwOperator
  _opSymbol <- symbol
  _opFixity <- symbol
  return OperatorSyntaxDef {..}

iteratorInfoNew ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r IteratorInfoNew
iteratorInfoNew = do
  l <- kw delimBraceL
  (_piteratorInfoInitNum, _piteratorInfoRangeNum) <- intercalateEffect semicolon $ do
    ini <- toPermutationWithDefault Nothing (Just <$> pinit)
    ran <- toPermutationWithDefault Nothing (Just <$> prangeNum)
    pure (ini, ran)
  r <- kw delimBraceR
  let _piteratorInfoBraces = Irrelevant (l, r)
  return IteratorInfoNew {..}
  where
    pinit :: ParsecS r (WithLoc Int)
    pinit = do
      void (kw kwInit >> kw kwAssign)
      fmap fromIntegral <$> integer

    prangeNum :: ParsecS r (WithLoc Int)
    prangeNum = do
      void (kw kwRange >> kw kwAssign)
      fmap fromIntegral <$> integer

iteratorSyntaxDef :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => KeywordRef -> ParsecS r IteratorSyntaxDef
iteratorSyntaxDef _iterSyntaxKw = do
  _iterIteratorKw <- kw kwIterator
  _iterSymbol <- symbol
  _iterInfo <- optional iteratorInfoNew
  return IteratorSyntaxDef {..}

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: forall r. (Members '[Files, PathResolver, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen, Error ParserError] r) => ParsecS r (Import 'Parsed)
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

importedModule :: forall r. (Members '[PathResolver, InfoTableBuilder, NameIdGen, Files, Error ParserError] r) => TopModulePath -> Sem r ()
importedModule t = unlessM (moduleVisited t) go
  where
    go :: Sem r ()
    go = withPath' t $ \path -> do
      visitModule t
      txt <- readFile' path
      eitherM throw (const (return ())) (runModuleParser path txt)

recordUpdateField :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RecordUpdateField 'Parsed)
recordUpdateField = do
  _fieldUpdateName <- symbol
  _fieldUpdateAssignKw <- Irrelevant <$> kw kwAssign
  _fieldUpdateValue <- parseExpressionAtoms
  let _fieldUpdateArgIx = ()
  return RecordUpdateField {..}

recordUpdate :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RecordUpdate 'Parsed)
recordUpdate = do
  _recordUpdateAtKw <- Irrelevant <$> kw kwAt
  _recordUpdateTypeName <- name
  l <- kw delimBraceL
  _recordUpdateFields <- P.sepEndBy1 recordUpdateField semicolon
  r <- kw delimBraceR
  let _recordUpdateDelims = Irrelevant (l, r)
      _recordUpdateExtra = Irrelevant ()
  return RecordUpdate {..}

expressionAtom :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  P.label "<expression>" $
    AtomLiteral
      <$> P.try literal
      <|> either AtomIterator AtomNamedApplication
        <$> iterator
      <|> AtomNamedApplication
        <$> namedApplication
      <|> AtomList
        <$> parseList
      <|> AtomIdentifier
        <$> name
      <|> AtomUniverse
        <$> universe
      <|> AtomLambda
        <$> lambda
      <|> AtomCase
        <$> case_
      <|> AtomFunction
        <$> function
      <|> AtomLet
        <$> letBlock
      <|> AtomFunArrow
        <$> kw kwRightArrow
      <|> AtomHole
        <$> hole
      <|> AtomParens
        <$> parens parseExpressionAtoms
      <|> AtomBraces
        <$> withLoc (braces parseExpressionAtoms)
      <|> AtomRecordUpdate
        <$> recordUpdate

parseExpressionAtoms ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- second Irrelevant <$> interval (P.some expressionAtom)
  return ExpressionAtoms {..}

--------------------------------------------------------------------------------
-- Iterators
--------------------------------------------------------------------------------

iterator ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (Either (Iterator 'Parsed) (NamedApplication 'Parsed))
iterator = do
  off <- P.getOffset
  (firstIsInit, keywordRef, _iteratorName, pat) <- P.try $ do
    n <- name
    lparen
    pat <- parsePatternAtoms
    (isInit, kwr) <-
      (True,)
        <$> kw kwAssign
        <|> (False,)
          <$> kw kwIn
    return (isInit, Irrelevant kwr, n, pat)
  val <- parseExpressionAtoms
  _iteratorInitializers <-
    if
        | firstIsInit -> do
            inis <- many (semicolon >> initializer)
            rparen
            let ini =
                  Initializer
                    { _initializerPattern = pat,
                      _initializerAssignKw = keywordRef,
                      _initializerExpression = val
                    }
            return (ini : inis)
        | otherwise -> return []
  _iteratorRanges <-
    if
        | not firstIsInit -> do
            rngs <- many (semicolon >> range)
            rparen
            let ran =
                  Range
                    { _rangeExpression = val,
                      _rangePattern = pat,
                      _rangeInKw = keywordRef
                    }
            return (ran : rngs)
        | otherwise -> fmap (maybe [] toList) . optional $ do
            s <- P.try $ do
              lparen
              rangeStart
            r <- rangeCont s
            rngs <- (r :) <$> many (semicolon >> range)
            rparen
            return rngs
  if
      | null _iteratorRanges -> do
          args <- nonEmpty' <$> mapM (mkNamedArgument off) _iteratorInitializers
          tailBlocks <- many argumentBlock
          let firstBlock =
                ArgumentBlock
                  { _argBlockDelims = Irrelevant Nothing,
                    _argBlockImplicit = Explicit,
                    _argBlockArgs = args
                  }
              _namedAppName = _iteratorName
              _namedAppArgs = firstBlock :| tailBlocks
              _namedAppSignature = Irrelevant ()
          return $ Right NamedApplication {..}
      | otherwise -> do
          (_iteratorBody, _iteratorBodyBraces) <-
            (,True)
              <$> braces parseExpressionAtoms
              <|> (,False)
                <$> parseExpressionAtoms
          let _iteratorParens = False
          return $ Left Iterator {..}
  where
    initializer :: ParsecS r (Initializer 'Parsed)
    initializer = do
      (_initializerPattern, _initializerAssignKw) <- P.try $ do
        pat <- parsePatternAtoms
        r <- Irrelevant <$> kw kwAssign
        return (pat, r)
      _initializerExpression <- parseExpressionAtoms
      return Initializer {..}

    rangeStart :: ParsecS r (PatternAtoms 'Parsed, Irrelevant KeywordRef)
    rangeStart = do
      pat <- parsePatternAtoms
      r <- Irrelevant <$> kw kwIn
      return (pat, r)

    rangeCont :: (PatternAtoms 'Parsed, Irrelevant KeywordRef) -> ParsecS r (Range 'Parsed)
    rangeCont (_rangePattern, _rangeInKw) = do
      _rangeExpression <- parseExpressionAtoms
      return Range {..}

    range :: ParsecS r (Range 'Parsed)
    range = do
      s <- P.try rangeStart
      rangeCont s

    mkNamedArgument :: Int -> Initializer 'Parsed -> ParsecS r (NamedArgument 'Parsed)
    mkNamedArgument off Initializer {..} = do
      let _namedArgAssignKw = _initializerAssignKw
          _namedArgValue = _initializerExpression
      _namedArgName <- case _initializerPattern ^. patternAtoms of
        PatternAtomIden (NameUnqualified n) :| [] -> return n
        _ -> parseFailure off "an iterator must have at least one range"
      return NamedArgument {..}

namedApplication ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (NamedApplication 'Parsed)
namedApplication = P.label "<named application>" $ do
  (_namedAppName, firstBlockStart) <- P.try $ do
    n <- name
    bs <- argumentBlockStart
    return (n, bs)
  firstBlock <- argumentBlockCont firstBlockStart
  tailBlocks <- many argumentBlock
  let _namedAppArgs = firstBlock :| tailBlocks
      _namedAppSignature = Irrelevant ()
  return NamedApplication {..}

namedArgument ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (NamedArgument 'Parsed)
namedArgument = do
  _namedArgName <- symbol
  _namedArgAssignKw <- Irrelevant <$> kw kwAssign
  _namedArgValue <- parseExpressionAtoms
  return NamedArgument {..}

argumentBlockStart ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (KeywordRef, IsImplicit, Symbol, Irrelevant KeywordRef)
argumentBlockStart = do
  (l, impl) <- implicitOpen
  n <- symbol
  a <- Irrelevant <$> kw kwAssign
  return (l, impl, n, a)

argumentBlockCont ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  (KeywordRef, IsImplicit, Symbol, Irrelevant KeywordRef) ->
  ParsecS r (ArgumentBlock 'Parsed)
argumentBlockCont (l, _argBlockImplicit, _namedArgName, _namedArgAssignKw) = do
  _namedArgValue <- parseExpressionAtoms
  let arg = NamedArgument {..}
  _argBlockArgs <- nonEmpty' . (arg :) <$> many (semicolon >> namedArgument)
  r <- implicitClose _argBlockImplicit
  let _argBlockDelims = Irrelevant (Just (l, r))
  return ArgumentBlock {..}

argumentBlock ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (ArgumentBlock 'Parsed)
argumentBlock = do
  s <- P.try argumentBlockStart
  argumentBlockCont s

hole :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (HoleType 'Parsed)
hole = kw kwHole

parseListPattern :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ListPattern 'Parsed)
parseListPattern = do
  _listpBracketL <- Irrelevant <$> kw kwBracketL
  _listpItems <- P.sepBy parsePatternAtoms (kw delimSemicolon)
  _listpBracketR <- Irrelevant <$> kw kwBracketR
  return ListPattern {..}

parseList :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (List 'Parsed)
parseList = do
  _listBracketL <- Irrelevant <$> kw kwBracketL
  _listItems <- P.sepBy parseExpressionAtoms (kw delimSemicolon)
  _listBracketR <- Irrelevant <$> kw kwBracketR
  return List {..}

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r LiteralLoc
literalInteger = fmap LitInteger <$> integer

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

letFunDef ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  ParsecS r (FunctionDef 'Parsed)
letFunDef = do
  optional_ stashPragmas
  functionDefinition Nothing

letStatement :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (LetStatement 'Parsed)
letStatement =
  LetFunctionDef
    <$> letFunDef
    <|> LetAliasDef
      <$> (kw kwSyntax >>= aliasDef)

letBlock :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Let 'Parsed)
letBlock = do
  _letKw <- kw kwLet
  _letFunDefs <- P.sepEndBy1 letStatement semicolon
  _letInKw <- Irrelevant <$> kw kwIn
  _letExpression <- parseExpressionAtoms
  return Let {..}

caseBranch :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (CaseBranch 'Parsed)
caseBranch = do
  _caseBranchPipe <- Irrelevant <$> kw kwPipe
  _caseBranchPattern <- parsePatternAtoms
  _caseBranchAssignKw <- Irrelevant <$> kw kwAssign
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
  i <- kw kwType
  lvl :: Maybe (WithLoc Natural) <- fmap (uncurry (flip WithLoc)) <$> optional decimal
  return
    Universe
      { _universeLevel = (^. withLocParam) <$> lvl,
        _universeKw = i,
        _universeLevelLoc = getLoc <$> lvl
      }

peekJudoc :: (Member JudocStash r) => ParsecS r (Maybe (Judoc 'Parsed))
peekJudoc = P.lift get

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

functionDefinition ::
  forall r.
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  Maybe (WithLoc BuiltinFunction) ->
  ParsecS r (FunctionDef 'Parsed)
functionDefinition _signBuiltin = P.label "<function definition>" $ do
  _signTerminating <- optional (kw kwTerminating)
  _signName <- symbol
  _signArgs <- many parseArg
  _signColonKw <- Irrelevant <$> kw kwColon
  _signRetType <- parseExpressionAtoms
  _signDoc <- getJudoc
  _signPragmas <- getPragmas
  _signBody <- parseBody
  return FunctionDef {..}
  where
    parseArg :: ParsecS r (SigArg 'Parsed)
    parseArg = do
      (openDelim, _sigArgNames, _sigArgImplicit, colonMay) <- P.try $ do
        (opn, impl) <- implicitOpen
        let parseArgumentName :: ParsecS r (Argument 'Parsed) =
              ArgumentSymbol
                <$> symbol
                <|> ArgumentWildcard
                  <$> wildcard
        n <- some1 parseArgumentName
        c <- case impl of
          Implicit -> optional (Irrelevant <$> kw kwColon)
          Explicit -> Just . Irrelevant <$> kw kwColon
        return (opn, n, impl, c)
      _sigArgRhs <- mapM parseRhs colonMay
      closeDelim <- implicitClose _sigArgImplicit
      let _sigArgDelims = Irrelevant (openDelim, closeDelim)
      return SigArg {..}
      where
        parseRhs :: Irrelevant KeywordRef -> ParsecS r (SigArgRhs 'Parsed)
        parseRhs _sigArgColon = do
          _sigArgType <- parseExpressionAtoms
          return SigArgRhs {..}

    parseBody :: ParsecS r (FunctionDefBody 'Parsed)
    parseBody =
      SigBodyExpression
        <$> bodyExpr
        <|> (SigBodyClauses <$> bodyClauses)
      where
        bodyClause :: ParsecS r (NewFunctionClause 'Parsed)
        bodyClause = do
          _clausenPipeKw <- Irrelevant <$> kw kwPipe
          _clausenPatterns <- some1 patternAtom
          _clausenAssignKw <- Irrelevant <$> kw kwAssign
          _clausenBody <- parseExpressionAtoms
          return NewFunctionClause {..}
        bodyClauses :: ParsecS r (NonEmpty (NewFunctionClause 'Parsed))
        bodyClauses = some1 bodyClause
        bodyExpr :: ParsecS r (ExpressionAtoms 'Parsed)
        bodyExpr = do
          void (kw kwAssign)
          parseExpressionAtoms

axiomDef ::
  (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) =>
  Maybe (WithLoc BuiltinAxiom) ->
  ParsecS r (AxiomDef 'Parsed)
axiomDef _axiomBuiltin = do
  _axiomKw <- Irrelevant <$> kw kwAxiom
  _axiomDoc <- getJudoc
  _axiomPragmas <- getPragmas
  _axiomName <- symbol
  _axiomColonKw <- Irrelevant <$> kw kwColon
  _axiomType <- parseExpressionAtoms
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

implicitOpen :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (KeywordRef, IsImplicit)
implicitOpen =
  (,Implicit)
    <$> kw delimBraceL
    <|> (,Explicit)
      <$> kw delimParenL

implicitClose :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => IsImplicit -> ParsecS r KeywordRef
implicitClose = \case
  Implicit -> kw delimBraceR
  Explicit -> kw delimParenR

functionParams :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (FunctionParameters 'Parsed)
functionParams = do
  (openDelim, _paramNames, _paramImplicit, _paramColon) <- P.try $ do
    (opn, impl) <- implicitOpen
    n <- some pName
    c <- Irrelevant . Just <$> kw kwColon
    return (opn, n, impl, c)
  _paramType <- parseExpressionAtoms
  closeDelim <- implicitClose _paramImplicit
  let _paramDelims = Irrelevant (Just (openDelim, closeDelim))
  return FunctionParameters {..}
  where
    pName :: ParsecS r (FunctionParameter 'Parsed)
    pName =
      FunctionParameterName
        <$> symbol
        <|> FunctionParameterWildcard
          <$> kw kwWildcard

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
  _lambdaAssignKw <- Irrelevant <$> kw kwAssign
  _lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Lambda 'Parsed)
lambda = do
  _lambdaKw <- kw kwLambda
  brl <- kw delimBraceL
  _lambdaClauses <- pipeSep1 lambdaClause
  brr <- kw delimBraceR
  let _lambdaBraces = Irrelevant (brl, brr)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

inductiveDef :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Maybe (WithLoc BuiltinInductive) -> ParsecS r (InductiveDef 'Parsed)
inductiveDef _inductiveBuiltin = do
  _inductivePositive <- optional (kw kwPositive)
  _inductiveKw <- Irrelevant <$> kw kwInductive
  _inductiveDoc <- getJudoc
  _inductivePragmas <- getPragmas
  _inductiveName <- symbol P.<?> "<type name>"
  _inductiveParameters <-
    P.many inductiveParams
      P.<?> "<type parameter e.g. '(A : Type)'>"
  _inductiveType <-
    optional (kw kwColon >> parseExpressionAtoms)
      P.<?> "<type annotation e.g. ': Type'>"
  _inductiveAssignKw <- Irrelevant <$> kw kwAssign P.<?> "<assignment symbol ':='>"
  _inductiveConstructors <-
    pipeSep1 constructorDef
      P.<?> "<constructor definition>"
  return InductiveDef {..}

inductiveParamsLong :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParamsLong = parens $ do
  _inductiveParametersNames <- some1 symbol
  colonMay <- optional (Irrelevant <$> kw kwColon)
  _inductiveParametersRhs <- mapM parseRhs colonMay
  return InductiveParameters {..}
  where
    parseRhs :: Irrelevant KeywordRef -> ParsecS r (InductiveParametersRhs 'Parsed)
    parseRhs _inductiveParametersColon = do
      _inductiveParametersType <- parseExpressionAtoms
      return InductiveParametersRhs {..}

inductiveParamsShort :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParamsShort = do
  _inductiveParametersNames <- some1 symbol
  return
    InductiveParameters
      { _inductiveParametersRhs = Nothing,
        ..
      }

inductiveParams :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParams = inductiveParamsLong <|> inductiveParamsShort

rhsGadt :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RhsGadt 'Parsed)
rhsGadt = P.label "<constructor gadt>" $ do
  _rhsGadtColon <- Irrelevant <$> kw kwColon
  _rhsGadtType <- parseExpressionAtoms P.<?> "<constructor type>"
  return RhsGadt {..}

recordField :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RecordField 'Parsed)
recordField = do
  _fieldName <- symbol
  _fieldColon <- Irrelevant <$> kw kwColon
  _fieldType <- parseExpressionAtoms
  return RecordField {..}

rhsAdt :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RhsAdt 'Parsed)
rhsAdt = P.label "<constructor arguments>" $ do
  _rhsAdtArguments <- many atomicExpression
  return RhsAdt {..}

rhsRecord :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RhsRecord 'Parsed)
rhsRecord = P.label "<constructor record>" $ do
  l <- kw delimBraceL
  _rhsRecordFields <- P.sepEndBy1 recordField semicolon
  r <- kw delimBraceR
  let _rhsRecordDelim = Irrelevant (l, r)
  return RhsRecord {..}

pconstructorRhs :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ConstructorRhs 'Parsed)
pconstructorRhs =
  ConstructorRhsGadt
    <$> rhsGadt
    <|> ConstructorRhsRecord
      <$> rhsRecord
    <|> ConstructorRhsAdt
      <$> rhsAdt

constructorDef :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (ConstructorDef 'Parsed)
constructorDef _constructorPipe = do
  _constructorDoc <- optional stashJudoc >> getJudoc
  _constructorPragmas <- optional stashPragmas >> getPragmas
  _constructorName <- symbol P.<?> "<constructor name>"
  _constructorRhs <- pconstructorRhs
  return ConstructorDef {..}

wildcard :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval (kw kwWildcard)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternAtomAnon :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomAnon =
  PatternAtomWildcard
    <$> wildcard
    <|> PatternAtomParens
      <$> parens parsePatternAtomsNested
    <|> PatternAtomBraces
      <$> braces parsePatternAtomsNested
    <|> PatternAtomList
      <$> parseListPattern

patternAtomAt :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Symbol -> ParsecS r PatternBinding
patternAtomAt s = do
  void (kw kwAt)
  PatternBinding s <$> patternAtom

recordPatternItem :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (RecordPatternItem 'Parsed)
recordPatternItem = do
  f <- symbol
  RecordPatternItemAssign
    <$> recordPatternItemAssign f
    <|> return (RecordPatternItemFieldPun (fieldPun f))
  where
    recordPatternItemAssign :: Symbol -> ParsecS r (RecordPatternAssign 'Parsed)
    recordPatternItemAssign f = do
      _recordPatternAssignKw <- Irrelevant <$> kw kwAssign
      pat' <- parsePatternAtomsNested
      return
        RecordPatternAssign
          { _recordPatternAssignField = f,
            _recordPatternAssignFieldIx = (),
            _recordPatternAssignPattern = pat',
            ..
          }
    fieldPun :: Symbol -> FieldPun 'Parsed
    fieldPun f =
      FieldPun
        { _fieldPunIx = (),
          _fieldPunField = f
        }

patternAtomRecord :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Name -> ParsecS r (RecordPattern 'Parsed)
patternAtomRecord _recordPatternConstructor = do
  -- The try is needed to disambiguate from `at` pattern
  P.try (void (kw kwAt >> kw delimBraceL))
  _recordPatternItems <- P.sepEndBy recordPatternItem semicolon
  kw delimBraceR
  return
    RecordPattern
      { _recordPatternSignature = Irrelevant (),
        ..
      }

-- | A pattern that starts with an identifier
patternAtomNamed :: forall r. (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtomNamed nested = do
  off <- P.getOffset
  n <- name
  case n of
    NameQualified {} ->
      PatternAtomRecord
        <$> patternAtomRecord n
        <|> return (PatternAtomIden n)
    NameUnqualified s -> do
      checkWrongEq off s
      PatternAtomRecord
        <$> patternAtomRecord n
        <|> PatternAtomAt
          <$> patternAtomAt s
        <|> return (PatternAtomIden n)
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
patternAtom' nested =
  P.label "<pattern>" $
    patternAtomNamed nested
      <|> patternAtomAnon

parsePatternAtoms :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtom)
  return PatternAtoms {..}

parsePatternAtomsNested :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtomsNested = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtomNested)
  return PatternAtoms {..}

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
  semicolon
  _moduleBody <- P.sepEndBy statement semicolon
  _moduleKwEnd <- endModule
  return Module {..}
  where
    _moduleInductive :: ModuleInductiveType t
    _moduleInductive = case sing :: SModuleIsTop t of
      SModuleLocal -> False
      SModuleTop -> ()

    endModule :: ParsecS r (ModuleEndType t)
    endModule = case sing :: SModuleIsTop t of
      SModuleLocal -> kw kwEnd
      SModuleTop -> optional_ (kw kwEnd >> semicolon)

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: (Members '[InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (ExpressionAtoms 'Parsed)
atomicExpression = do
  (atom, loc) <- interval expressionAtom
  case atom of
    AtomFunArrow {} -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom) (Irrelevant loc)

openModule :: forall r. (Members '[Error ParserError, PathResolver, Files, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (OpenModule 'Parsed)
openModule = do
  _openModuleKw <- kw kwOpen
  _openModuleName <- name
  _openUsingHiding <- optional usingOrHiding
  _openPublicKw <- Irrelevant <$> optional (kw kwPublic)
  let _openPublic = maybe NoPublic (const Public) (_openPublicKw ^. unIrrelevant)
      _openModuleImportKw :: Maybe KeywordRef
      _openModuleImportKw = Nothing
  return
    OpenModule
      { _openImportAsName = Nothing,
        ..
      }

usingOrHiding :: (Members '[Error ParserError, InfoTableBuilder, JudocStash, NameIdGen, PragmasStash] r) => ParsecS r (UsingHiding 'Parsed)
usingOrHiding =
  Using
    <$> pusingList
    <|> Hiding
      <$> phidingList

openSyntax :: forall r. (Members '[Error ParserError, PathResolver, Files, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => Import 'Parsed -> ParsecS r (OpenModule 'Parsed)
openSyntax im = do
  _openModuleKw <- kw kwOpen
  _openUsingHiding <- optional usingOrHiding
  _openPublicKw <- Irrelevant <$> optional (kw kwPublic)
  let _openModuleName = topModulePathToName (im ^. importModule)
      _openModuleImportKw = Just (im ^. importKw)
      _openImportAsName = im ^. importAsName
      _openPublic = maybe NoPublic (const Public) (_openPublicKw ^. unIrrelevant)
  return OpenModule {..}

importOpenSyntax :: forall r. (Members '[Error ParserError, PathResolver, Files, InfoTableBuilder, PragmasStash, JudocStash, NameIdGen] r) => ParsecS r (Either (Import 'Parsed) (OpenModule 'Parsed))
importOpenSyntax = do
  im <- import_
  (Right <$> openSyntax im) <|> return (Left im)
