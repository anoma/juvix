module Juvix.Compiler.Concrete.Translation.FromSource
  ( module Juvix.Compiler.Concrete.Translation.FromSource,
    module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Parser.Error,
  )
where

import Commonmark qualified as MK
import Control.Applicative.Permutations
import Data.ByteString.UTF8 qualified as BS
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Singletons
import Data.Text qualified as Text
import Juvix.Compiler.Backend.Markdown.Data.Types (Mk (..))
import Juvix.Compiler.Backend.Markdown.Data.Types qualified as MK
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Concrete (HighlightBuilder, ignoreHighlightBuilder)
import Juvix.Compiler.Concrete.Extra (takeWhile1P)
import Juvix.Compiler.Concrete.Extra qualified as P
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource.Data.Context
import Juvix.Compiler.Concrete.Translation.FromSource.Lexer hiding
  ( symbol,
  )
import Juvix.Compiler.Concrete.Translation.FromSource.ParserResultBuilder
import Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker
import Juvix.Data.Yaml
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Parser.Error
import Juvix.Prelude
import Juvix.Prelude.Pretty
  ( Pretty,
    prettyText,
  )

data MdModuleBuilder = MdModuleBuilder
  { _mdModuleBuilder :: Module 'Parsed 'ModuleTop,
    _mdModuleBuilderBlocksLengths :: [Int]
  }

makeLenses ''MdModuleBuilder

type JudocStash = State (Maybe (Judoc 'Parsed))

type PragmasStash = State (Maybe ParsedPragmas)

fromSource ::
  (Members '[HighlightBuilder, TopModuleNameChecker, Files, Error JuvixError] r) =>
  Maybe Text ->
  Maybe (Path Abs File) ->
  Sem r ParserResult
fromSource mstdin minputfile = mapError (JuvixError @ParserError) $ do
  (_resultParserState, _resultModule) <- runParserResultBuilder mempty getParsedModuleTop
  return ParserResult {..}
  where
    getParsedModuleTop ::
      forall r.
      (Members '[Files, TopModuleNameChecker, Error ParserError, ParserResultBuilder] r) =>
      Sem r (Module 'Parsed 'ModuleTop)
    getParsedModuleTop = case (mstdin, minputfile) of
      (Nothing, Nothing) -> throw $ ErrStdinOrFile StdinOrFileError
      (Just txt, Just x) ->
        runModuleParser x txt >>= \case
          Left err -> throw err
          Right r -> return r
      (Just txt, Nothing) ->
        runModuleStdinParser txt >>= \case
          Left err -> throw err
          Right r -> return r
      (Nothing, Just x) -> goFile x

    goFile ::
      forall r.
      (Members '[Files, TopModuleNameChecker, Error ParserError, ParserResultBuilder] r) =>
      Path Abs File ->
      Sem r (Module 'Parsed 'ModuleTop)
    goFile fileName = do
      input_ <- getFileContents fileName
      mp <- runModuleParser fileName input_
      case mp of
        Left er -> throw er
        Right m -> return m
      where
        getFileContents :: Path Abs File -> Sem r Text
        getFileContents fp
          | Just fp == minputfile,
            Just txt <- mstdin =
              return txt
          | otherwise = readFile' fp

data ReplInput
  = ReplExpression (ExpressionAtoms 'Parsed)
  | ReplImport (Import 'Parsed)
  | ReplOpen (OpenModule 'Parsed 'OpenFull)

expressionFromTextSource ::
  (Members '[Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionFromTextSource fp txt = mapError (JuvixError @ParserError) $ do
  exp <- runExpressionParser fp txt
  case exp of
    Left e -> throw e
    Right exp' -> return exp'

replInputFromTextSource ::
  (Members '[Error JuvixError, ParserResultBuilder] r) =>
  Path Abs File ->
  Text ->
  Sem r ReplInput
replInputFromTextSource fp txt = mapError (JuvixError @ParserError) $ runReplInputParser fp txt

runReplInputParser ::
  (Members '[Error ParserError, ParserResultBuilder] r) =>
  Path Abs File ->
  Text ->
  Sem r ReplInput
runReplInputParser fileName input_ = do
  m <-
    evalState (Nothing @ParsedPragmas) $
      evalState (Nothing @(Judoc 'Parsed)) $
        P.runParserT replInput (toFilePath fileName) input_
  case m of
    Left err -> throw (ErrMegaparsec (MegaparsecError err))
    Right r -> return r

runModuleParser ::
  (Members '[ParserResultBuilder, TopModuleNameChecker, Error ParserError] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleParser fileName input_
  | isJuvixMarkdownFile fileName = do
      res <- P.runParserT juvixCodeBlockParser (toFilePath fileName) input_
      case res of
        Left err -> return . Left . ErrMegaparsec . MegaparsecError $ err
        Right r
          | MK.nullMk r ->
              return . Left . ErrMarkdownBackend $
                ErrNoJuvixCodeBlocks NoJuvixCodeBlocksError {_noJuvixCodeBlocksErrorFilepath = fileName}
          | otherwise -> runMarkdownModuleParser fileName r
  | otherwise = do
      m <-
        evalState (Nothing @ParsedPragmas)
          . evalState (Nothing @(Judoc 'Parsed))
          $ P.runParserT topModuleDef (toFilePath fileName) input_
      case m of
        Left err -> return . Left . ErrMegaparsec . MegaparsecError $ err
        Right r -> return $ Right r

runMarkdownModuleParser ::
  (Members '[ParserResultBuilder] r) =>
  Path Abs File ->
  Mk ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runMarkdownModuleParser fpath mk =
  runError $ case nonEmpty (MK.extractJuvixCodeBlock mk) of
    Nothing ->
      throw
        ( ErrMarkdownBackend $
            ErrNoJuvixCodeBlocks
              NoJuvixCodeBlocksError
                { _noJuvixCodeBlocksErrorFilepath = fpath
                }
        )
    Just (firstBlock :| restBlocks) -> do
      m0 <- parseFirstBlock firstBlock
      let iniBuilder =
            MdModuleBuilder
              { _mdModuleBuilder = m0,
                _mdModuleBuilderBlocksLengths = [length (m0 ^. moduleBody)]
              }
      res <- runInputList restBlocks (execState iniBuilder parseRestBlocks)
      let m =
            set
              moduleMarkdownInfo
              ( Just
                  MarkdownInfo
                    { _markdownInfo = mk,
                      _markdownInfoBlockLengths = reverse (res ^. mdModuleBuilderBlocksLengths)
                    }
              )
              $ res ^. mdModuleBuilder
      return m
  where
    getInitPos :: Interval -> P.SourcePos
    getInitPos i =
      P.SourcePos
        { P.sourceName = fromAbsFile $ i ^. intervalFile,
          P.sourceLine = P.mkPos (intervalStartLine i),
          P.sourceColumn = P.mkPos (intervalStartCol i)
        }

    getInitialParserState :: forall a. MK.JuvixCodeBlock -> P.State Text a
    getInitialParserState code =
      let initPos =
            maybe
              (P.initialPos (toFilePath fpath))
              getInitPos
              (code ^. MK.juvixCodeBlockInterval)
       in P.State
            { P.stateInput = code ^. MK.juvixCodeBlock,
              P.statePosState =
                P.PosState
                  { P.pstateInput = code ^. MK.juvixCodeBlock,
                    P.pstateOffset = 0,
                    P.pstateSourcePos = initPos,
                    P.pstateTabWidth = P.defaultTabWidth,
                    P.pstateLinePrefix = ""
                  },
              P.stateOffset = 0,
              P.stateParseErrors = []
            }
    parseHelper ::
      (Members '[Error ParserError] r') =>
      P.ParsecT Void Text (Sem (JudocStash ': PragmasStash ': r')) a ->
      MK.JuvixCodeBlock ->
      Sem r' a
    parseHelper p x = do
      res <-
        fmap snd
          . evalState (Nothing @ParsedPragmas)
          . evalState (Nothing @(Judoc 'Parsed))
          $ P.runParserT' p (getInitialParserState x)
      case res of
        Left err -> throw . ErrMegaparsec . MegaparsecError $ err
        Right m -> return m

    parseFirstBlock ::
      (Members '[ParserResultBuilder, Error ParserError] r') =>
      MK.JuvixCodeBlock ->
      Sem r' (Module 'Parsed 'ModuleTop)
    parseFirstBlock x = parseHelper topMarkdownModuleDef x

    parseRestBlocks ::
      forall r'.
      (Members '[ParserResultBuilder, Error ParserError, Input MK.JuvixCodeBlock, State MdModuleBuilder] r') =>
      Sem r' ()
    parseRestBlocks = whenJustM input $ \x -> do
      stmts <- parseHelper parseTopStatements x
      modify' (over (mdModuleBuilder . moduleBody) (<> stmts))
      modify' (over mdModuleBuilderBlocksLengths (length stmts :))
      parseRestBlocks

runModuleStdinParser ::
  (Members '[Error ParserError, ParserResultBuilder] r) =>
  Text ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
runModuleStdinParser input_ = do
  m <-
    evalState (Nothing @ParsedPragmas)
      . evalState (Nothing @(Judoc 'Parsed))
      $ P.runParserT topModuleDefStdin (toFilePath formatStdinPath) input_
  case m of
    Left err -> return (Left (ErrMegaparsec (MegaparsecError err)))
    Right r -> return $ Right r

runExpressionParser ::
  Path Abs File ->
  Text ->
  Sem r (Either ParserError (ExpressionAtoms 'Parsed))
runExpressionParser fpath input_ = do
  m <-
    ignoreHighlightBuilder
      $ runParserResultBuilder mempty
        . evalState (Nothing @ParsedPragmas)
        . evalState (Nothing @(Judoc 'Parsed))
      $ P.runParserT parseExpressionAtoms (toFilePath fpath) input_
  case m of
    (_, Left err) -> return (Left (ErrMegaparsec (MegaparsecError err)))
    (_, Right r) -> return (Right r)

-- | The first pipe is optional, and thus we need a `Maybe`. The rest of the elements are guaranteed to be given a `Just`.
pipeSep1 :: (Member ParserResultBuilder r) => (Irrelevant (Maybe KeywordRef) -> ParsecS r a) -> ParsecS r (NonEmpty a)
pipeSep1 e = do
  p <- Irrelevant <$> optional (kw kwPipe)
  h <- e p
  (h :|) <$> many (kw kwPipe >>= e . Irrelevant . Just)

top ::
  (Member ParserResultBuilder r) =>
  ParsecS r a ->
  ParsecS r a
top p = space >> p <* (optional semicolon >> P.eof)

topModuleDefStdin ::
  (Members '[Error ParserError, ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDefStdin = do
  optional_ stashJudoc
  top moduleDef

topModuleDef ::
  (Members '[Error ParserError, TopModuleNameChecker, ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDef = do
  space >> optional_ stashJudoc
  optional_ stashPragmas
  m <- top moduleDef
  P.lift (checkModulePath (m ^. modulePath))
  return m

juvixCodeBlockParser ::
  ParsecS r Mk
juvixCodeBlockParser = do
  ls :: [Mk] <-
    many $
      goJuvixCodeBlock
        <|> MK.MkTextBlock <$> goTextBlock
  return $ foldl' (<>) MkNull ls
  where
    mdCodeToken :: ParsecS r Text
    mdCodeToken = P.string "```"

    goValidText :: ParsecS r (WithLoc Text)
    goValidText = do
      p <- withLoc $ toList <$> P.some (P.notFollowedBy mdCodeToken >> P.anySingle)
      return $
        WithLoc
          { _withLocInt = getLoc p,
            _withLocParam = Text.pack $ p ^. withLocParam
          }

    goTextBlock :: ParsecS r MK.TextBlock
    goTextBlock = do
      w <- goValidText
      return $
        MK.TextBlock
          { _textBlock = w ^. withLocParam,
            _textBlockInterval = Just $ getLoc w
          }

    goJuvixCodeBlock :: ParsecS r MK.Mk
    goJuvixCodeBlock = do
      void mdCodeToken
      info :: Text <- Text.pack <$> P.manyTill P.anySingle (P.lookAhead (P.string "\n"))
      t <- goValidText
      void mdCodeToken
      return $
        MK.processCodeBlock
          info
          (t ^. withLocParam)
          (Just $ t ^. withLocInt)

-- Keep it. Intended to be used later for processing Markdown inside TextBlocks
-- or (Judoc) comments.
commanMarkParser ::
  (Members '[ParserResultBuilder, Error ParserError] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either ParserError (Module 'Parsed 'ModuleTop))
commanMarkParser fileName input_ = do
  res <- MK.commonmarkWith MK.defaultSyntaxSpec (toFilePath fileName) input_
  case res of
    Right (r :: Mk) -> runMarkdownModuleParser fileName r
    Left r -> return . Left . ErrCommonmark . CommonmarkError $ r

topMarkdownModuleDef ::
  (Members '[ParserResultBuilder, Error ParserError, PragmasStash, JudocStash] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topMarkdownModuleDef = do
  optional_ stashJudoc
  optional_ stashPragmas
  top moduleDef

parseTopStatements ::
  forall r.
  (Members '[ParserResultBuilder, Error ParserError, PragmasStash, JudocStash] r) =>
  ParsecS r [Statement 'Parsed]
parseTopStatements = top $ P.sepEndBy statement semicolon

replInput :: forall r. (Members '[ParserResultBuilder, JudocStash, Error ParserError, State (Maybe ParsedPragmas)] r) => ParsecS r ReplInput
replInput =
  P.label "<repl input>" $
    ReplExpression <$> parseExpressionAtoms
      <|> ReplOpen <$> openModule
      <|> ReplImport <$> import_

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r Symbol
symbol = uncurry (flip WithLoc) <$> identifierL

dottedSymbol :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry (flip WithLoc)) <$> dottedIdentifier

name :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (SymbolPath p) n)
    (Nothing, n) -> NameUnqualified n

mkTopModulePath :: NonEmpty Symbol -> TopModulePath
mkTopModulePath l = TopModulePath (NonEmpty.init l) (NonEmpty.last l)

usingItem :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (UsingItem 'Parsed)
usingItem = do
  _usingModuleKw <- optional (kw kwModule)
  _usingSymbol <- symbol
  alias <- optional $ do
    k <- Irrelevant <$> kw kwAs
    (k,) <$> symbol
  let _usingAsKw = mapM fst alias
      _usingAs = snd <$> alias
  return UsingItem {..}

hidingItem :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (HidingItem 'Parsed)
hidingItem = do
  _hidingModuleKw <- optional (kw kwModule)
  _hidingSymbol <- symbol
  return HidingItem {..}

phidingList :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (HidingList 'Parsed)
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

pusingList :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (UsingList 'Parsed)
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

topModulePath :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r TopModulePath
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

statement :: (Members '[Error ParserError, ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Statement 'Parsed)
statement = P.label "<top level statement>" $ do
  optional_ stashJudoc
  optional_ stashPragmas
  ms <-
    optional
      ( StatementImport <$> import_
          <|> StatementOpenModule <$> openModule
          <|> StatementSyntax <$> syntaxDef
          <|> StatementInductive <$> inductiveDef Nothing
          <|> StatementModule <$> moduleDef
          <|> StatementAxiom <$> axiomDef Nothing
          <|> builtinStatement
          <|> StatementFunctionDef <$> functionDefinition False True Nothing
      )
  case ms of
    Just s -> return s
    Nothing -> do
      mj <- peekJudoc
      case mj of
        Nothing -> P.failure Nothing mempty
        Just j -> P.lift . throw . ErrDanglingJudoc . DanglingJudoc $ j

stashPragmas :: forall r. (Members '[ParserResultBuilder, PragmasStash] r) => ParsecS r ()
stashPragmas = do
  pragmas <- withLoc parsePragmas
  P.lift (registerPragmas (getLoc pragmas))
  P.lift (put (Just pragmas))
  where
    parsePragmas :: ParsecS r (WithSource Pragmas)
    parsePragmas = parseYaml Str.pragmasStart Str.pragmasEnd

parseYaml :: (Member ParserResultBuilder r, FromJSON a) => Text -> Text -> ParsecS r (WithSource a)
parseYaml l r = do
  void (P.chunk l)
  off <- P.getOffset
  str <- P.manyTill P.anySingle (P.chunk r)
  let str'
        | '\n' `elem` str = str
        | otherwise = '{' : str ++ "}"
  space
  let bs = BS.fromString str'
  case decodeEither bs of
    Left err -> parseFailure off (prettyPrintParseException err)
    Right yaml -> return $ WithSource (fromString str) yaml

stashJudoc :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r ()
stashJudoc = do
  b <- judoc
  many (judocEmptyLine False)
  P.lift (modify (<> Just b))
  where
    judoc :: ParsecS r (Judoc 'Parsed)
    judoc = Judoc <$> some1 judocGroup

    judocGroup :: ParsecS r (JudocGroup 'Parsed)
    judocGroup =
      JudocGroupBlock <$> judocParagraphBlock
        <|> JudocGroupLines <$> some1 (judocBlock False)

    judocEmptyLine :: Bool -> ParsecS r ()
    judocEmptyLine inBlock =
      lexeme . void $
        if
            | inBlock -> P.newline
            | otherwise -> P.try (judocStart >> P.newline)

    judocBlock :: Bool -> ParsecS r (JudocBlock 'Parsed)
    judocBlock inBlock = do
      p <- judocParagraphLines inBlock
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

    paragraphLine :: Bool -> ParsecS r (JudocLine 'Parsed)
    paragraphLine inBlock = do
      kwstart <-
        if
            | inBlock -> return Nothing
            | otherwise -> P.try $ do
                s <- judocStart
                P.notFollowedBy (P.choice [void P.newline])
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
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  Bool ->
  ParsecS r (JudocAtom 'Parsed)
judocAtom inBlock =
  JudocText <$> judocAtomText
    <|> JudocExpression <$> judocExpression
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

builtinInductive :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (WithLoc BuiltinInductive)
builtinInductive = builtinHelper

builtinFunction :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (WithLoc BuiltinFunction)
builtinFunction = builtinHelper

builtinAxiom :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (WithLoc BuiltinAxiom)
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r (WithLoc a)
builtinHelper =
  P.choice
    [ (`WithLoc` a) <$> onlyInterval (kw (asciiKw (prettyText a)))
      | a <- allElements
    ]

builtinInductiveDef :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => WithLoc BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  WithLoc BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinFunctionDef ::
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  WithLoc BuiltinFunction ->
  ParsecS r (FunctionDef 'Parsed)
builtinFunctionDef = functionDefinition False True . Just

builtinStatement :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Statement 'Parsed)
builtinStatement = do
  void (kw kwBuiltin)
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementFunctionDef . builtinFunctionDef)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

builtinRecordField :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (WithLoc BuiltinFunction)
builtinRecordField = do
  void (kw kwBuiltin)
  builtinFunction

--------------------------------------------------------------------------------
-- Syntax declaration
--------------------------------------------------------------------------------

syntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (SyntaxDef 'Parsed)
syntaxDef = do
  syn <- kw kwSyntax
  SyntaxFixity <$> fixitySyntaxDef syn
    <|> SyntaxOperator <$> operatorSyntaxDef syn
    <|> SyntaxIterator <$> iteratorSyntaxDef syn
    <|> SyntaxAlias <$> aliasDef syn

aliasDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> ParsecS r (AliasDef 'Parsed)
aliasDef synKw = do
  let _aliasDefSyntaxKw = Irrelevant synKw
  _aliasDefAliasKw <- Irrelevant <$> kw kwAlias
  _aliasDefName <- symbol
  kw kwAssign
  _aliasDefAsName <- name
  return AliasDef {..}

parsedFixityFields ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (ParsedFixityFields 'Parsed)
parsedFixityFields = do
  l <- kw delimBraceL
  (_fixityFieldsAssoc, _fixityFieldsPrecBelow, _fixityFieldsPrecAbove, _fixityFieldsPrecSame) <- intercalateEffect semicolon $ do
    as <- toPermutationWithDefault Nothing (Just <$> assoc)
    bel <- toPermutationWithDefault Nothing (Just <$> belowAbove kwBelow)
    abov <- toPermutationWithDefault Nothing (Just <$> belowAbove kwAbove)
    sam <- toPermutationWithDefault Nothing (Just <$> same)
    pure (as, bel, abov, sam)
  r <- kw delimBraceR
  let _fixityFieldsBraces = Irrelevant (l, r)
  return ParsedFixityFields {..}
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
      kw kwLeft
        $> AssocLeft
        <|> kw kwRight
          $> AssocRight
        <|> kw kwNone
          $> AssocNone

parsedFixityInfo :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ParsedFixityInfo 'Parsed)
parsedFixityInfo = do
  _fixityParsedArity <- withLoc ari
  _fixityFields <- optional parsedFixityFields
  return ParsedFixityInfo {..}
  where
    ari :: ParsecS r Arity
    ari =
      kw kwUnary
        $> Unary
        <|> kw kwBinary
          $> Binary
        <|> kw kwNone
          $> None

fixitySyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> ParsecS r (FixitySyntaxDef 'Parsed)
fixitySyntaxDef _fixitySyntaxKw = P.label "<fixity declaration>" $ do
  _fixityDoc <- getJudoc
  _fixityKw <- kw kwFixity
  _fixitySymbol <- symbol
  _fixityAssignKw <- kw kwAssign
  _fixityInfo <- parsedFixityInfo
  return FixitySyntaxDef {..}

operatorSyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> ParsecS r OperatorSyntaxDef
operatorSyntaxDef _opSyntaxKw = do
  _opKw <- kw kwOperator
  _opSymbol <- symbol
  _opFixity <- symbol
  return OperatorSyntaxDef {..}

parsedIteratorInfo ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r ParsedIteratorInfo
parsedIteratorInfo = do
  l <- kw delimBraceL
  (_parsedIteratorInfoInitNum, _parsedIteratorInfoRangeNum) <- intercalateEffect semicolon $ do
    ini <- toPermutationWithDefault Nothing (Just <$> pinit)
    ran <- toPermutationWithDefault Nothing (Just <$> prangeNum)
    pure (ini, ran)
  r <- kw delimBraceR
  let _parsedIteratorInfoBraces = Irrelevant (l, r)
  return ParsedIteratorInfo {..}
  where
    pinit :: ParsecS r (WithLoc Int)
    pinit = do
      void (kw kwInit >> kw kwAssign)
      fmap fromIntegral <$> integer

    prangeNum :: ParsecS r (WithLoc Int)
    prangeNum = do
      void (kw kwRange >> kw kwAssign)
      fmap fromIntegral <$> integer

iteratorSyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> ParsecS r IteratorSyntaxDef
iteratorSyntaxDef _iterSyntaxKw = do
  _iterIteratorKw <- kw kwIterator
  _iterSymbol <- symbol
  _iterInfo <- optional parsedIteratorInfo
  return IteratorSyntaxDef {..}

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash, Error ParserError] r) => ParsecS r (Import 'Parsed)
import_ = do
  _importKw <- kw kwImport
  _importModulePath <- topModulePath
  _importAsName <- optional pasName
  _importUsingHiding <- optional usingOrHiding
  _importPublic <- publicAnn
  _importOpen <- optional openModule
  let i = Import {..}
  P.lift (registerImport i)
  return i
  where
    pasName :: ParsecS r TopModulePath
    pasName = void (kw kwAs) >> topModulePath

recordUpdateField :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RecordUpdateField 'Parsed)
recordUpdateField = do
  _fieldUpdateName <- symbol
  _fieldUpdateAssignKw <- Irrelevant <$> kw kwAssign
  _fieldUpdateValue <- parseExpressionAtoms
  let _fieldUpdateArgIx = ()
  return RecordUpdateField {..}

recordUpdate :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RecordUpdate 'Parsed)
recordUpdate = do
  _recordUpdateAtKw <- Irrelevant <$> kw kwAt
  _recordUpdateTypeName <- name
  l <- kw delimBraceL
  _recordUpdateFields <- P.sepEndBy recordUpdateField semicolon
  r <- kw delimBraceR
  let _recordUpdateDelims = Irrelevant (l, r)
      _recordUpdateExtra = Irrelevant ()
  return RecordUpdate {..}

expressionAtom :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  P.label "<expression>" $
    AtomLiteral <$> P.try literal
      <|> either AtomIterator AtomNamedApplication <$> iterator
      <|> AtomNamedApplicationNew <$> namedApplicationNew
      <|> AtomNamedApplication <$> namedApplication
      <|> AtomList <$> parseList
      <|> either AtomIf AtomIdentifier <$> multiwayIf
      <|> AtomIdentifier <$> name
      <|> AtomUniverse <$> universe
      <|> AtomLambda <$> lambda
      <|> AtomCase <$> case_
      <|> AtomFunction <$> function
      <|> AtomLet <$> letBlock
      <|> AtomFunArrow <$> kw kwRightArrow
      <|> AtomHole <$> hole
      <|> AtomParens <$> parens parseExpressionAtoms
      <|> AtomDoubleBraces <$> pdoubleBracesExpression
      <|> AtomBraces <$> withLoc (braces parseExpressionAtoms)
      <|> AtomRecordUpdate <$> recordUpdate

parseExpressionAtoms ::
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- second Irrelevant <$> interval (P.some expressionAtom)
  return ExpressionAtoms {..}

pdoubleBracesExpression ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (DoubleBracesExpression 'Parsed)
pdoubleBracesExpression = do
  l <- kw delimDoubleBraceL
  _doubleBracesExpression <- instanceHole <|> parseExpressionAtoms
  r <- kw delimDoubleBraceR
  return
    DoubleBracesExpression
      { _doubleBracesDelims = Irrelevant (l, r),
        ..
      }
  where
    instanceHole :: ParsecS r (ExpressionAtoms 'Parsed)
    instanceHole = do
      (h, i) <- interval (kw kwHole)
      return $
        ExpressionAtoms
          { _expressionAtoms = NonEmpty.singleton (AtomInstanceHole h),
            _expressionAtomsLoc = Irrelevant i
          }

--------------------------------------------------------------------------------
-- Iterators
--------------------------------------------------------------------------------

iterator ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (Either (Iterator 'Parsed) (NamedApplication 'Parsed))
iterator = do
  off <- P.getOffset
  (firstIsInit, keywordRef, _iteratorName, pat) <- P.try $ do
    n <- name
    lparen
    pat <- parsePatternAtoms
    (isInit, kwr) <-
      (True,) <$> kw kwAssign
        <|> (False,) <$> kw kwIn
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
            (,True) <$> braces parseExpressionAtoms
              <|> (,False) <$> parseExpressionAtoms
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

namedApplicationNew ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (NamedApplicationNew 'Parsed)
namedApplicationNew = P.label "<named application>" $ do
  (_namedApplicationNewName, _namedApplicationNewAtKw, _namedApplicationNewExhaustive) <- P.try $ do
    n <- name
    (a, b) <- first Irrelevant <$> ((,False) <$> kw kwAtQuestion <|> (,True) <$> kw kwAt)
    lbrace
    return (n, a, b)
  defs <- P.sepEndBy (functionDefinition True False Nothing) semicolon
  rbrace
  let _namedApplicationNewArguments = fmap mkArg defs
      _namedApplicationNewExtra = Irrelevant ()
  return NamedApplicationNew {..}
  where
    mkArg :: FunctionDef 'Parsed -> NamedArgumentNew 'Parsed
    mkArg f =
      NamedArgumentNew
        { _namedArgumentNewFunDef = f
        }

namedApplication ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
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
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (NamedArgument 'Parsed)
namedArgument = do
  _namedArgName <- symbol
  _namedArgAssignKw <- Irrelevant <$> kw kwAssign
  _namedArgValue <- parseExpressionAtoms
  return NamedArgument {..}

argumentBlockStart ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (KeywordRef, IsImplicit, Symbol, Irrelevant KeywordRef)
argumentBlockStart = do
  (l, impl) <- implicitOpen
  n <- symbol
  a <- Irrelevant <$> kw kwAssign
  return (l, impl, n, a)

argumentBlockCont ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
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
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (ArgumentBlock 'Parsed)
argumentBlock = do
  s <- P.try argumentBlockStart
  argumentBlockCont s

hole :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (HoleType 'Parsed)
hole = kw kwHole

parseListPattern :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ListPattern 'Parsed)
parseListPattern = do
  _listpBracketL <- Irrelevant <$> kw kwBracketL
  _listpItems <- P.sepBy parsePatternAtoms (kw delimSemicolon)
  _listpBracketR <- Irrelevant <$> kw kwBracketR
  return ListPattern {..}

parseList :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (List 'Parsed)
parseList = do
  _listBracketL <- Irrelevant <$> kw kwBracketL
  _listItems <- P.sepBy parseExpressionAtoms (kw delimSemicolon)
  _listBracketR <- Irrelevant <$> kw kwBracketR
  return List {..}

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r LiteralLoc
literalInteger = fmap LitIntegerWithBase <$> integerWithBase

literalString :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (WithLoc loc (LitString x))

literal :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

letFunDef ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  ParsecS r (FunctionDef 'Parsed)
letFunDef = do
  optional_ stashPragmas
  functionDefinition True False Nothing

letStatement :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (LetStatement 'Parsed)
letStatement =
  LetFunctionDef <$> letFunDef
    <|> LetAliasDef <$> (kw kwSyntax >>= aliasDef)
    <|> LetOpen <$> openModule

letBlock :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Let 'Parsed)
letBlock = do
  _letKw <- kw kwLet
  _letFunDefs <- P.sepEndBy1 letStatement semicolon
  _letInKw <- Irrelevant <$> kw kwIn
  _letExpression <- parseExpressionAtoms
  return Let {..}

caseBranch :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (CaseBranch 'Parsed)
caseBranch _caseBranchPipe = do
  _caseBranchPattern <- parsePatternAtoms
  _caseBranchAssignKw <- Irrelevant <$> kw kwAssign
  _caseBranchExpression <- parseExpressionAtoms
  return CaseBranch {..}

case_ :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Case 'Parsed)
case_ = P.label "case" $ do
  _caseKw <- kw kwCase
  _caseExpression <- parseExpressionAtoms
  _caseOfKw <- kw kwOf
  _caseBranches <- braces (pipeSep1 caseBranch) <|> pipeSep1 caseBranch
  return Case {..}

ifBranch' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Irrelevant KeywordRef -> ParsecS r (IfBranch 'Parsed)
ifBranch' _ifBranchPipe = do
  _ifBranchCondition <- parseExpressionAtoms
  _ifBranchAssignKw <- Irrelevant <$> kw kwAssign
  _ifBranchExpression <- parseExpressionAtoms
  return IfBranch {..}

parseIfBranchElse' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Irrelevant KeywordRef -> ParsecS r (IfBranchElse 'Parsed)
parseIfBranchElse' _ifBranchElsePipe = do
  _ifBranchElseKw <- Irrelevant <$> kw kwElse
  _ifBranchElseAssignKw <- Irrelevant <$> kw kwAssign
  _ifBranchElseExpression <- parseExpressionAtoms
  return IfBranchElse {..}

multiwayIf' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> [IfBranch 'Parsed] -> ParsecS r (If 'Parsed)
multiwayIf' _ifKw brs = do
  pipeKw <- Irrelevant <$> kw kwPipe
  multiwayIfBranchElse' _ifKw pipeKw brs <|> multiwayIfBranch' _ifKw pipeKw brs

multiwayIfBranch' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> Irrelevant KeywordRef -> [IfBranch 'Parsed] -> ParsecS r (If 'Parsed)
multiwayIfBranch' _ifKw pipeKw brs = do
  br <- ifBranch' pipeKw
  multiwayIf' _ifKw (br : brs)

multiwayIfBranchElse' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => KeywordRef -> Irrelevant KeywordRef -> [IfBranch 'Parsed] -> ParsecS r (If 'Parsed)
multiwayIfBranchElse' _ifKw pipeKw brs = do
  off <- P.getOffset
  _ifBranchElse <- parseIfBranchElse' pipeKw
  case nonEmpty (reverse brs) of
    Nothing -> parseFailure off "A multiway if must have at least one condition branch"
    Just _ifBranches -> return If {..}

multiwayIf :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Either (If 'Parsed) Name)
multiwayIf = do
  _ifKw <- kw kwIf
  (Left <$> multiwayIf' _ifKw [])
    <|> return (Right $ NameUnqualified $ WithLoc (getLoc _ifKw) (_ifKw ^. keywordRefKeyword . keywordAscii))

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r Universe
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
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
  Bool ->
  Bool ->
  Maybe (WithLoc BuiltinFunction) ->
  ParsecS r (FunctionDef 'Parsed)
functionDefinition allowOmitType allowInstance _signBuiltin = P.label "<function definition>" $ do
  _signTerminating <- optional (kw kwTerminating)
  off <- P.getOffset
  _signCoercion <- optional (kw kwCoercion)
  unless (allowInstance || isNothing _signCoercion) $
    parseFailure off "coercion not allowed here"
  off0 <- P.getOffset
  _signInstance <- optional (kw kwInstance)
  unless (allowInstance || isNothing _signInstance) $
    parseFailure off0 "instance not allowed here"
  when (isJust _signCoercion && isNothing _signInstance) $
    parseFailure off0 "expected: instance"
  _signName <- symbol
  _signArgs <- many parseArg
  off' <- P.getOffset
  _signColonKw <-
    Irrelevant
      <$> if
          | allowOmitType -> optional (kw kwColon)
          | otherwise -> Just <$> kw kwColon
  _signRetType <-
    case _signColonKw ^. unIrrelevant of
      Just {} -> Just <$> parseExpressionAtoms
      Nothing -> return Nothing
  _signDoc <- getJudoc
  _signPragmas <- getPragmas
  _signBody <- parseBody
  unless
    ( isJust (_signColonKw ^. unIrrelevant)
        || (P.isBodyExpression _signBody && null _signArgs)
    )
    $ parseFailure off' "expected result type"
  return FunctionDef {..}
  where
    parseArg :: ParsecS r (SigArg 'Parsed)
    parseArg = do
      (openDelim, _sigArgNames, _sigArgImplicit, _sigArgColon) <- P.try $ do
        (opn, impl) <- implicitOpen
        let parseArgumentName :: ParsecS r (Argument 'Parsed) =
              ArgumentSymbol <$> symbol
                <|> ArgumentWildcard <$> wildcard
        let parseArgumentNameColon :: ParsecS r (Argument 'Parsed, Irrelevant KeywordRef) = P.try $ do
              n <- parseArgumentName
              c <- Irrelevant <$> kw kwColon
              return (n, c)
        (ns, c) <- case impl of
          ImplicitInstance -> do
            ma <- optional parseArgumentNameColon
            return $ case ma of
              Just (ns', c') -> ([ns'], Just c')
              Nothing -> ([], Nothing)
          Implicit -> do
            ns <- some parseArgumentName
            c <- optional (Irrelevant <$> kw kwColon)
            return (ns, c)
          Explicit -> do
            ns <- some parseArgumentName
            c <- Just . Irrelevant <$> kw kwColon
            return (ns, c)
        return (opn, ns, impl, c)
      _sigArgType <- case _sigArgImplicit of
        Implicit
          | isNothing _sigArgColon ->
              return Nothing
        _ ->
          Just <$> parseExpressionAtoms
      _sigArgDefault <- optional $ do
        _argDefaultAssign <- Irrelevant <$> kw kwAssign
        _argDefaultValue <- parseExpressionAtoms
        return ArgDefault {..}
      closeDelim <- implicitClose _sigArgImplicit
      let _sigArgDelims = Irrelevant (openDelim, closeDelim)
      return SigArg {..}

    parseBody :: ParsecS r (FunctionDefBody 'Parsed)
    parseBody =
      SigBodyExpression <$> bodyExpr
        <|> (SigBodyClauses <$> bodyClauses)
      where
        bodyClause :: ParsecS r (FunctionClause 'Parsed)
        bodyClause = do
          _clausenPipeKw <- Irrelevant <$> kw kwPipe
          _clausenPatterns <- some1 patternAtom
          _clausenAssignKw <- Irrelevant <$> kw kwAssign
          _clausenBody <- parseExpressionAtoms
          return FunctionClause {..}
        bodyClauses :: ParsecS r (NonEmpty (FunctionClause 'Parsed))
        bodyClauses = some1 bodyClause
        bodyExpr :: ParsecS r (ExpressionAtoms 'Parsed)
        bodyExpr = do
          void (kw kwAssign)
          parseExpressionAtoms

axiomDef ::
  (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) =>
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

implicitOpen :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (KeywordRef, IsImplicit)
implicitOpen =
  (,ImplicitInstance) <$> kw delimDoubleBraceL
    <|> (,Implicit) <$> kw delimBraceL
    <|> (,Explicit) <$> kw delimParenL

implicitClose :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => IsImplicit -> ParsecS r KeywordRef
implicitClose = \case
  Implicit -> kw delimBraceR
  Explicit -> kw delimParenR
  ImplicitInstance -> kw delimDoubleBraceR

functionParams :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (FunctionParameters 'Parsed)
functionParams = do
  (openDelim, _paramNames, _paramImplicit, _paramColon) <- P.try $ do
    (opn, impl) <- implicitOpen
    case impl of
      ImplicitInstance -> do
        n <- pName <* kw kwColon
        return (opn, [n], impl, Irrelevant Nothing)
      _ -> do
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
      FunctionParameterName <$> symbol
        <|> FunctionParameterWildcard <$> kw kwWildcard

function :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Function 'Parsed)
function = do
  _funParameters <- functionParams
  _funKw <- kw kwRightArrow
  _funReturn <- parseExpressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (LambdaClause 'Parsed)
lambdaClause _lambdaPipe = do
  _lambdaParameters <- P.some patternAtom
  _lambdaAssignKw <- Irrelevant <$> kw kwAssign
  _lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Lambda 'Parsed)
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

inductiveDef :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Maybe (WithLoc BuiltinInductive) -> ParsecS r (InductiveDef 'Parsed)
inductiveDef _inductiveBuiltin = do
  _inductivePositive <- optional (kw kwPositive)
  _inductiveTrait <- optional (kw kwTrait)
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
    pipeSep1 (constructorDef _inductiveName)
      P.<?> "<constructor definition>"
  return InductiveDef {..}

inductiveParamsLong :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
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

inductiveParamsShort :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParamsShort = do
  _inductiveParametersNames <- some1 symbol
  return
    InductiveParameters
      { _inductiveParametersRhs = Nothing,
        ..
      }

inductiveParams :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParams = inductiveParamsLong <|> inductiveParamsShort

rhsGadt :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RhsGadt 'Parsed)
rhsGadt = P.label "<constructor gadt>" $ do
  _rhsGadtColon <- Irrelevant <$> kw kwColon
  _rhsGadtType <- parseExpressionAtoms P.<?> "<constructor type>"
  return RhsGadt {..}

recordField :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RecordField 'Parsed)
recordField = do
  _fieldBuiltin <- optional builtinRecordField
  _fieldName <- symbol
  _fieldColon <- Irrelevant <$> kw kwColon
  _fieldType <- parseExpressionAtoms
  return RecordField {..}

rhsAdt :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RhsAdt 'Parsed)
rhsAdt = P.label "<constructor arguments>" $ do
  _rhsAdtArguments <- many atomicExpression
  return RhsAdt {..}

rhsRecord :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RhsRecord 'Parsed)
rhsRecord = P.label "<constructor record>" $ do
  l <- kw delimBraceL
  _rhsRecordStatements <- P.sepEndBy recordStatement semicolon
  r <- kw delimBraceR
  let _rhsRecordDelim = Irrelevant (l, r)
  return RhsRecord {..}

recordStatement :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RecordStatement 'Parsed)
recordStatement =
  RecordStatementOperator <$> operator
    <|> RecordStatementField <$> recordField
  where
    operator :: ParsecS r OperatorSyntaxDef
    operator = do
      syn <- kw kwSyntax
      operatorSyntaxDef syn

pconstructorRhs :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ConstructorRhs 'Parsed)
pconstructorRhs =
  ConstructorRhsGadt <$> rhsGadt
    <|> ConstructorRhsRecord <$> rhsRecord
    <|> ConstructorRhsAdt <$> rhsAdt

constructorDef :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Symbol -> Irrelevant (Maybe KeywordRef) -> ParsecS r (ConstructorDef 'Parsed)
constructorDef _constructorInductiveName _constructorPipe = do
  _constructorDoc <- optional stashJudoc >> getJudoc
  _constructorPragmas <- optional stashPragmas >> getPragmas
  _constructorName <- symbol P.<?> "<constructor name>"
  _constructorRhs <- pconstructorRhs
  return ConstructorDef {..}

wildcard :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval (kw kwWildcard)

patternAtomWildcardConstructor :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (WildcardConstructor 'Parsed)
patternAtomWildcardConstructor = P.try $ do
  _wildcardConstructor <- name
  _wildcardConstructorAtKw <- Irrelevant <$> kw kwAt
  l <- kw delimBraceL
  r <- kw delimBraceR
  let _wildcardConstructorDelims = Irrelevant (l, r)
  return WildcardConstructor {..}

patternAtomAnon :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomAnon =
  PatternAtomWildcard <$> wildcard
    <|> PatternAtomDoubleBraces <$> doubleBraces parsePatternAtomsNested
    <|> PatternAtomParens <$> parens parsePatternAtomsNested
    <|> PatternAtomBraces <$> braces parsePatternAtomsNested
    <|> PatternAtomList <$> parseListPattern

patternAtomAt :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Symbol -> ParsecS r PatternBinding
patternAtomAt _patternBindingName = do
  _patternBindingAtKw <- Irrelevant <$> kw kwAt
  _patternBindingPattern <- patternAtom
  return PatternBinding {..}

recordPatternItem :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (RecordPatternItem 'Parsed)
recordPatternItem = do
  f <- symbol
  RecordPatternItemAssign <$> recordPatternItemAssign f
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

patternAtomRecord :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Name -> ParsecS r (RecordPattern 'Parsed)
patternAtomRecord _recordPatternConstructor = do
  -- The try is needed to disambiguate from `at` pattern
  P.try (void (kw kwAt >> kw delimBraceL))
  _recordPatternItems <- P.sepEndBy recordPatternItem semicolon
  kw delimBraceR
  return
    RecordPattern {..}

-- | A pattern that starts with an identifier
patternAtomNamed :: forall r. (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtomNamed nested = do
  off <- P.getOffset
  n <- name
  case n of
    NameQualified {} ->
      PatternAtomRecord <$> patternAtomRecord n
        <|> return (PatternAtomIden n)
    NameUnqualified s -> do
      checkWrongEq off s
      PatternAtomRecord <$> patternAtomRecord n
        <|> PatternAtomAt <$> patternAtomAt s
        <|> return (PatternAtomIden n)
  where
    checkWrongEq :: Int -> WithLoc Text -> ParsecS r ()
    checkWrongEq off t =
      when
        (not nested && t ^. withLocParam == "=")
        (parseFailure off "expected \":=\" instead of \"=\"")

patternAtomNested :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomNested = patternAtom' True

patternAtom :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtom = patternAtom' False

patternAtom' :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtom' nested =
  P.label "<pattern>" $
    PatternAtomWildcardConstructor <$> patternAtomWildcardConstructor
      <|> patternAtomNamed nested
      <|> patternAtomAnon

parsePatternAtoms :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtom)
  return PatternAtoms {..}

parsePatternAtomsNested :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtomsNested = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtomNested)
  return PatternAtoms {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: forall t r. (SingI t, Members '[Error ParserError, ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (Module 'Parsed t)
moduleDef = P.label "<module definition>" $ do
  _moduleKw <- kw kwModule
  _moduleDoc <- getJudoc
  _modulePragmas <- getPragmas
  _modulePath <- pmodulePath
  semicolon
  _moduleBody <- P.sepEndBy statement semicolon
  _moduleKwEnd <- endModule
  let _moduleId = ()
  return
    Module
      { _moduleMarkdownInfo = Nothing,
        ..
      }
  where
    _moduleOrigin :: ModuleInductiveType t
    _moduleOrigin = case sing :: SModuleIsTop t of
      SModuleLocal -> LocalModuleSource
      SModuleTop -> ()

    endModule :: ParsecS r (ModuleEndType t)
    endModule = case sing :: SModuleIsTop t of
      SModuleLocal -> kw kwEnd
      SModuleTop -> optional_ (kw kwEnd >> semicolon)

-- | An ExpressionAtom which is a valid expression on its own.
atomicExpression :: (Members '[ParserResultBuilder, PragmasStash, JudocStash] r) => ParsecS r (ExpressionAtoms 'Parsed)
atomicExpression = do
  (atom, loc) <- interval expressionAtom
  case atom of
    AtomFunArrow {} -> P.failure Nothing mempty
    _ -> return ()
  return $ ExpressionAtoms (NonEmpty.singleton atom) (Irrelevant loc)

publicAnn :: forall r. (Members '[ParserResultBuilder] r) => ParsecS r PublicAnn
publicAnn = maybe NoPublic (Public . Irrelevant) <$> optional (kw kwPublic)

openModule ::
  forall r (short :: IsOpenShort).
  ( SingI short,
    Members
      '[ ParserResultBuilder,
         PragmasStash,
         JudocStash
       ]
      r
  ) =>
  ParsecS r (OpenModule 'Parsed short)
openModule = do
  _openModuleKw <- kw kwOpen
  _openModuleName <- case sing :: SIsOpenShort short of
    SOpenFull -> name
    SOpenShort -> return ()
  _openModuleUsingHiding <- optional usingOrHiding
  _openModulePublic <- publicAnn
  return OpenModule {..}

usingOrHiding :: (Members '[ParserResultBuilder, JudocStash, PragmasStash] r) => ParsecS r (UsingHiding 'Parsed)
usingOrHiding =
  Using <$> pusingList
    <|> Hiding <$> phidingList
