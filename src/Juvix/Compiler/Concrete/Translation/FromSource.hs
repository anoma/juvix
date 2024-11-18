module Juvix.Compiler.Concrete.Translation.FromSource
  ( module Juvix.Compiler.Concrete.Translation.FromSource.Data.Context,
    module Juvix.Parser.Error,
    expressionFromTextSource,
    runModuleParser,
    fromSource,
    replInputFromTextSource,
    ReplInput (..),
  )
where

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
import Juvix.Compiler.Concrete.Gen (mkExpressionAtoms)
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

data FunctionSyntaxOptions = FunctionSyntaxOptions
  { _funAllowOmitType :: Bool,
    _funAllowInstance :: Bool
  }

data SigOptions = SigOptions
  { _sigAllowOmitType :: Bool,
    _sigAllowDefault :: Bool
  }

data MdModuleBuilder = MdModuleBuilder
  { _mdModuleBuilder :: Module 'Parsed 'ModuleTop,
    _mdModuleBuilderBlocksLengths :: [Int]
  }

makeLenses ''MdModuleBuilder
makeLenses ''FunctionSyntaxOptions
makeLenses ''SigOptions

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

    getInitFileLoc :: Interval -> FileLoc
    getInitFileLoc = (^. intervalStart)

    getInitialParserState :: forall a. MK.JuvixCodeBlock -> P.State Text a
    getInitialParserState code =
      let initPos :: P.SourcePos =
            maybe
              (P.initialPos (toFilePath fpath))
              getInitPos
              (code ^. MK.juvixCodeBlockInterval)
          initFileLoc :: FileLoc =
            maybe
              mkInitialFileLoc
              getInitFileLoc
              (code ^. MK.juvixCodeBlockInterval)
       in P.State
            { P.stateInput = code ^. MK.juvixCodeBlock,
              P.statePosState =
                P.PosState
                  { P.pstateInput = code ^. MK.juvixCodeBlock,
                    P.pstateOffset = fromIntegral (initFileLoc ^. locOffset),
                    P.pstateSourcePos = initPos,
                    P.pstateTabWidth = P.defaultTabWidth,
                    P.pstateLinePrefix = ""
                  },
              P.stateOffset = fromIntegral (initFileLoc ^. locOffset),
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
      . evalParserResultBuilder mempty
      . evalState (Nothing @ParsedPragmas)
      . evalState (Nothing @(Judoc 'Parsed))
      . runError @ParserError
      $ P.runParserT parseExpressionAtoms (toFilePath fpath) input_
  return $ case m of
    Left err -> Left err
    Right x -> case x of
      Left merr -> Left (ErrMegaparsec (MegaparsecError merr))
      Right r -> return r

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
  (Members '[Error ParserError, ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topModuleDefStdin = do
  optional_ stashJudoc
  top moduleDef

topModuleDef ::
  (Members '[Error ParserError, TopModuleNameChecker, ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
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

topMarkdownModuleDef ::
  (Members '[ParserResultBuilder, Error ParserError, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (Module 'Parsed 'ModuleTop)
topMarkdownModuleDef = do
  optional_ stashJudoc
  optional_ stashPragmas
  top moduleDef

parseTopStatements ::
  forall r.
  (Members '[ParserResultBuilder, Error ParserError, PragmasStash, Error ParserError, JudocStash] r) =>
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

symbol :: (Members '[ParserResultBuilder] r) => ParsecS r Symbol
symbol = uncurry (flip WithLoc) <$> identifierL

dottedSymbol :: (Members '[ParserResultBuilder] r) => ParsecS r (NonEmpty Symbol)
dottedSymbol = fmap (uncurry (flip WithLoc)) <$> dottedIdentifier

name :: (Members '[ParserResultBuilder] r) => ParsecS r Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (SymbolPath p) n)
    (Nothing, n) -> NameUnqualified n

usingItem :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (UsingItem 'Parsed)
usingItem = do
  _usingModuleKw <- optional (kw kwModule)
  _usingSymbol <- symbol
  alias <- optional $ do
    k <- Irrelevant <$> kw kwAs
    (k,) <$> symbol
  let _usingAsKw = mapM fst alias
      _usingAs = snd <$> alias
  return UsingItem {..}

hidingItem :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (HidingItem 'Parsed)
hidingItem = do
  _hidingModuleKw <- optional (kw kwModule)
  _hidingSymbol <- symbol
  return HidingItem {..}

phidingList :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (HidingList 'Parsed)
phidingList = do
  _hidingKw <- Irrelevant <$> kw kwHiding
  l <- kw delimBraceL
  _hidingList <- P.sepEndBy1 hidingItem semicolon
  r <- kw delimBraceR
  return
    HidingList
      { _hidingBraces = Irrelevant (l, r),
        ..
      }

pusingList :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (UsingList 'Parsed)
pusingList = do
  _usingKw <- Irrelevant <$> kw kwUsing
  l <- kw delimBraceL
  _usingList <- P.sepEndBy1 usingItem semicolon
  r <- kw delimBraceR
  return
    UsingList
      { _usingBraces = Irrelevant (l, r),
        ..
      }

topModulePath :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r TopModulePath
topModulePath = mkTopModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

recoverStashes :: (Members '[PragmasStash, JudocStash] r) => ParsecS r a -> ParsecS r a
recoverStashes r = do
  p <- P.lift (get @(Maybe ParsedPragmas))
  j <- P.lift (get @(Maybe (Judoc 'Parsed)))
  res <- r
  P.lift $ do
    put p
    put j
  return res

statement :: (Members '[Error ParserError, ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Statement 'Parsed)
statement = P.label "<top level statement>" $ do
  optional_ stashJudoc
  optional_ stashPragmas
  let funSyntax =
        FunctionSyntaxOptions
          { _funAllowInstance = True,
            _funAllowOmitType = False
          }
  ms <-
    optional
      ( StatementImport <$> import_
          <|> StatementOpenModule <$> openModule
          <|> StatementSyntax <$> syntaxDef
          <|> StatementInductive <$> inductiveDef Nothing
          <|> StatementModule <$> moduleDef
          <|> StatementAxiom <$> axiomDef Nothing
          <|> builtinStatement
          <|> StatementFunctionDef <$> functionDefinition funSyntax Nothing
      )
  case ms of
    Just s -> return s
    Nothing -> do
      mj <- peekJudoc
      case mj of
        Nothing -> P.failure Nothing mempty
        Just j -> P.lift . throw . ErrDanglingJudoc . DanglingJudoc $ j

stashPragmas :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError] r) => ParsecS r ()
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

stashJudoc :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r ()
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
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
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

builtinInductive :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (WithLoc BuiltinInductive)
builtinInductive = builtinHelper

builtinFunction :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (WithLoc BuiltinFunction)
builtinFunction = builtinHelper

builtinAxiom :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (WithLoc BuiltinAxiom)
builtinAxiom = builtinHelper

builtinHelper ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r, Bounded a, Enum a, Pretty a) =>
  ParsecS r (WithLoc a)
builtinHelper =
  P.choice
    [ (`WithLoc` a) <$> onlyInterval (kw (asciiKw (prettyText a)))
      | a <- allElements
    ]

builtinInductiveDef :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => WithLoc BuiltinInductive -> ParsecS r (InductiveDef 'Parsed)
builtinInductiveDef = inductiveDef . Just

builtinAxiomDef ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  WithLoc BuiltinAxiom ->
  ParsecS r (AxiomDef 'Parsed)
builtinAxiomDef = axiomDef . Just

builtinFunctionDef ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  WithLoc BuiltinFunction ->
  ParsecS r (FunctionDef 'Parsed)
builtinFunctionDef = functionDefinition funSyntax . Just
  where
    funSyntax =
      FunctionSyntaxOptions
        { _funAllowInstance = True,
          _funAllowOmitType = False
        }

builtinStatement :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Statement 'Parsed)
builtinStatement = do
  void (kw kwBuiltin)
  (builtinInductive >>= fmap StatementInductive . builtinInductiveDef)
    <|> (builtinFunction >>= fmap StatementFunctionDef . builtinFunctionDef)
    <|> (builtinAxiom >>= fmap StatementAxiom . builtinAxiomDef)

builtinRecordField :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (WithLoc BuiltinFunction)
builtinRecordField = do
  void (kw kwBuiltin)
  builtinFunction

--------------------------------------------------------------------------------
-- Syntax declaration
--------------------------------------------------------------------------------

syntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (SyntaxDef 'Parsed)
syntaxDef = do
  syn <- kw kwSyntax
  SyntaxFixity <$> fixitySyntaxDef syn
    <|> SyntaxOperator <$> operatorSyntaxDef syn
    <|> SyntaxIterator <$> iteratorSyntaxDef syn
    <|> SyntaxAlias <$> aliasDef syn

aliasDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => KeywordRef -> ParsecS r (AliasDef 'Parsed)
aliasDef synKw = do
  let _aliasDefSyntaxKw = Irrelevant synKw
  _aliasDefAliasKw <- Irrelevant <$> kw kwAlias
  _aliasDefName <- symbol
  kw kwAssign
  _aliasDefAsName <- name
  return AliasDef {..}

parsedFixityFields ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (ParsedFixityFields 'Parsed)
parsedFixityFields = do
  l <- kw delimBraceL
  (_fixityFieldsAssoc, _fixityFieldsPrecBelow, _fixityFieldsPrecAbove, _fixityFieldsPrecSame) <- intercalateEffect semicolon $ do
    as <- toPermutationWithDefault Nothing (Just <$> assoc)
    bel <- toPermutationWithDefault Nothing (Just <$> belowAbove kwBelow)
    abov <- toPermutationWithDefault Nothing (Just <$> belowAbove kwAbove)
    sam <- toPermutationWithDefault Nothing (Just <$> same)
    -- This is needed to allow an optional semicolon at the end
    toPermutationWithDefault Nothing (return (Just ()))
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
      kw delimBracketL
      r <- P.sepEndBy symbol semicolon
      kw delimBracketR
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

parsedFixityInfo :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ParsedFixityInfo 'Parsed)
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

fixitySyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => KeywordRef -> ParsecS r (FixitySyntaxDef 'Parsed)
fixitySyntaxDef _fixitySyntaxKw = P.label "<fixity declaration>" $ do
  _fixityDoc <- getJudoc
  _fixityKw <- kw kwFixity
  _fixitySymbol <- symbol
  _fixityAssignKw <- kw kwAssign
  _fixityInfo <- parsedFixityInfo
  return FixitySyntaxDef {..}

operatorSyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => KeywordRef -> ParsecS r OperatorSyntaxDef
operatorSyntaxDef _opSyntaxKw = do
  _opKw <- kw kwOperator
  _opSymbol <- symbol
  _opFixity <- symbol
  return OperatorSyntaxDef {..}

parsedIteratorInfo ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r ParsedIteratorInfo
parsedIteratorInfo = do
  l <- kw delimBraceL
  (_parsedIteratorInfoInitNum, _parsedIteratorInfoRangeNum) <- intercalateEffect semicolon $ do
    ini <- toPermutationWithDefault Nothing (Just <$> pinit)
    ran <- toPermutationWithDefault Nothing (Just <$> prangeNum)
    -- This is needed to allow an optional semicolon at the end
    toPermutationWithDefault Nothing (return (Just ()))
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

iteratorSyntaxDef :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => KeywordRef -> ParsecS r IteratorSyntaxDef
iteratorSyntaxDef _iterSyntaxKw = do
  _iterIteratorKw <- kw kwIterator
  _iterSymbol <- symbol
  _iterInfo <- optional parsedIteratorInfo
  return IteratorSyntaxDef {..}

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash, Error ParserError] r) => ParsecS r (Import 'Parsed)
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

recordUpdateField :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RecordUpdateField 'Parsed)
recordUpdateField = do
  _fieldUpdateName <- symbol
  _fieldUpdateAssignKw <- Irrelevant <$> kw kwAssign
  _fieldUpdateValue <- parseExpressionAtoms
  let _fieldUpdateArgIx = ()
  return RecordUpdateField {..}

recordUpdate :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RecordUpdate 'Parsed)
recordUpdate = do
  _recordUpdateAtKw <- Irrelevant <$> kw kwAt
  _recordUpdateTypeName <- name
  l <- kw delimBraceL
  _recordUpdateFields <- P.sepEndBy recordUpdateField semicolon
  r <- kw delimBraceR
  let _recordUpdateDelims = Irrelevant (l, r)
      _recordUpdateExtra = Irrelevant ()
  return RecordUpdate {..}

expressionAtom :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ExpressionAtom 'Parsed)
expressionAtom =
  P.label "<expression>" $
    AtomLiteral <$> P.try literal
      <|> AtomIterator <$> iterator
      <|> AtomNamedApplicationNew <$> namedApplicationNew
      <|> AtomList <$> parseList
      <|> AtomIf <$> multiwayIf
      <|> AtomIdentifier <$> name
      <|> AtomFunction <$> function
      <|> AtomUniverse <$> universe
      <|> AtomLambda <$> lambda
      <|> AtomCase <$> case_
      <|> AtomLet <$> letBlock
      <|> AtomDo <$> doBlock
      <|> AtomFunArrow <$> kw kwRightArrow
      <|> AtomHole <$> hole
      <|> AtomParens <$> parens parseExpressionAtoms
      <|> AtomDoubleBraces <$> pdoubleBracesExpression
      <|> AtomRecordUpdate <$> recordUpdate
      <|> AtomBraces <$> withLoc (braces parseExpressionAtoms)

parseExpressionAtoms ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (ExpressionAtoms 'Parsed)
parseExpressionAtoms = do
  (_expressionAtoms, _expressionAtomsLoc) <- second Irrelevant <$> interval (P.some expressionAtom)
  return ExpressionAtoms {..}

pdoubleBracesExpression ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
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

iterator ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (Iterator 'Parsed)
iterator = do
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
  do
    (_iteratorBody, _iteratorBodyBraces) <-
      (,True) <$> braces parseExpressionAtoms
        <|> (,False) <$> parens parseExpressionAtoms
        <|> (,True) <$> parseExpressionAtoms
    let _iteratorParens = False
    return $ Iterator {..}
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

pnamedArgumentFunctionDef ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (NamedArgumentFunctionDef 'Parsed)
pnamedArgumentFunctionDef = do
  optional_ stashJudoc
  optional_ stashPragmas
  let funSyntax =
        FunctionSyntaxOptions
          { _funAllowOmitType = True,
            _funAllowInstance = False
          }
  fun <- functionDefinition funSyntax Nothing
  return
    NamedArgumentFunctionDef
      { _namedArgumentFunctionDef = fun
      }

pnamedArgumentItemPun ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (NamedArgumentPun 'Parsed)
pnamedArgumentItemPun = do
  sym <- symbol
  return
    NamedArgumentPun
      { _namedArgumentPunSymbol = sym,
        _namedArgumentReferencedSymbol = ()
      }

-- | Parses zero or more named arguments. This function is necessary to avoid
-- using excessive backtracking.
manyNamedArgumentNewRBrace ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r [NamedArgumentNew 'Parsed]
manyNamedArgumentNewRBrace = reverse <$> go []
  where
    go :: [NamedArgumentNew 'Parsed] -> ParsecS r [NamedArgumentNew 'Parsed]
    go acc =
      rbrace $> acc
        <|> itemHelper (P.try (withIsLast (NamedArgumentItemPun <$> pnamedArgumentItemPun)))
        <|> itemHelper (withIsLast (NamedArgumentNewFunction <$> pnamedArgumentFunctionDef))
      where
        itemHelper :: ParsecS r (Bool, NamedArgumentNew 'Parsed) -> ParsecS r [NamedArgumentNew 'Parsed]
        itemHelper p = do
          (isLast, item) <- p
          let acc' = item : acc
          if
              | isLast -> return acc'
              | otherwise -> go acc'

    pIsLast :: ParsecS r Bool
    pIsLast =
      rbrace $> True
        <|> semicolon $> False

    withIsLast :: ParsecS r a -> ParsecS r (Bool, a)
    withIsLast p = do
      res <- p
      isLast <- pIsLast
      return (isLast, res)

pisExhaustive ::
  forall r.
  (Members '[ParserResultBuilder] r) =>
  ParsecS r IsExhaustive
pisExhaustive = do
  (keyword, exh) <-
    (,False) <$> kw kwAtQuestion
      <|> (,True) <$> kw kwAt
  return
    IsExhaustive
      { _isExhaustiveKw = Irrelevant keyword,
        _isExhaustive = exh
      }

namedApplicationNew ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (NamedApplicationNew 'Parsed)
namedApplicationNew = P.label "<named application>" $ do
  checkNoNamedApplicationMissingAt
  (_namedApplicationNewName, _namedApplicationNewExhaustive) <- P.try $ do
    n <- name
    exhaustive <- pisExhaustive
    lbrace
    return (n, exhaustive)
  _namedApplicationNewArguments <- manyNamedArgumentNewRBrace
  let _namedApplicationNewExtra = Irrelevant ()
  return NamedApplicationNew {..}

hole :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (HoleType 'Parsed)
hole = kw kwHole

parseListPattern :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ListPattern 'Parsed)
parseListPattern = do
  _listpBracketL <- Irrelevant <$> kw delimBracketL
  _listpItems <- P.sepEndBy parsePatternAtoms (kw delimSemicolon)
  _listpBracketR <- Irrelevant <$> kw delimBracketR
  return ListPattern {..}

parseList :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (List 'Parsed)
parseList = do
  _listBracketL <- Irrelevant <$> kw delimBracketL
  _listItems <- P.sepEndBy parseExpressionAtoms (kw delimSemicolon)
  _listBracketR <- Irrelevant <$> kw delimBracketR
  return List {..}

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

literalInteger :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r LiteralLoc
literalInteger = fmap LitIntegerWithBase <$> integerWithBase

literalString :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r LiteralLoc
literalString = do
  (x, loc) <- string
  return (WithLoc loc (LitString x))

literal :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r LiteralLoc
literal = do
  l <-
    literalInteger
      <|> literalString
  P.lift (registerLiteral l)

letFunDef ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (FunctionDef 'Parsed)
letFunDef = do
  optional_ stashPragmas
  functionDefinition funSyntax Nothing
  where
    funSyntax =
      FunctionSyntaxOptions
        { _funAllowOmitType = True,
          _funAllowInstance = False
        }

letStatement :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (LetStatement 'Parsed)
letStatement =
  LetFunctionDef <$> letFunDef
    <|> LetAliasDef <$> (kw kwSyntax >>= aliasDef)
    <|> LetOpen <$> openModule

doBind :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (DoBind 'Parsed)
doBind = do
  (_doBindPattern, _doBindArrowKw) <- P.try $ do
    pat <- parsePatternAtoms
    arrowkw <- Irrelevant <$> kw kwLeftArrow
    return (pat, arrowkw)
  _doBindExpression <- parseExpressionAtoms
  return DoBind {..}

doLet :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (DoLet 'Parsed)
doLet = do
  _doLetKw <- Irrelevant <$> kw kwLet
  _doLetStatements <- P.sepEndBy1 letStatement semicolon
  _doLetInKw <- Irrelevant <$> kw kwIn
  return DoLet {..}

doStatements :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (NonEmpty (DoStatement 'Parsed))
doStatements =
  P.label "<do statement>" $
    plet
      <|> pother
  where
    plet :: ParsecS r (NonEmpty (DoStatement 'Parsed))
    plet = do
      s <- DoStatementLet <$> doLet
      ss <- doStatements
      return (s :| toList ss)

    pother :: ParsecS r (NonEmpty (DoStatement 'Parsed))
    pother = do
      s <-
        DoStatementBind <$> doBind
          <|> DoStatementExpression <$> parseExpressionAtoms
      semi <- isJust <$> optional semicolon
      if
          | semi -> do
              ss <- maybe [] toList <$> optional doStatements
              return (s :| ss)
          | otherwise -> return (pure s)

doBlock :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Do 'Parsed)
doBlock = do
  _doKeyword <- Irrelevant <$> kw kwDo
  lbr <- kw delimBraceL
  _doStatements <- doStatements
  rbr <- kw delimBraceR
  let
  return
    Do
      { _doDelims = Irrelevant (lbr, rbr),
        ..
      }

letBlock :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Let 'Parsed)
letBlock = do
  _letKw <- kw kwLet
  _letFunDefs <- P.sepEndBy1 letStatement semicolon
  _letInKw <- Irrelevant <$> kw kwIn
  _letExpression <- parseExpressionAtoms
  return Let {..}

-- | The pipe for the first branch is optional
sideIfBranch ::
  forall r k.
  (SingI k, Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  Bool ->
  ParsecS r (SideIfBranch 'Parsed k)
sideIfBranch isFirst = do
  let ifElseKw =
        Irrelevant <$> case sing :: SIfBranchKind k of
          SBranchIfBool -> kw kwIf
          SBranchIfElse -> kw kwElse
  (_sideIfBranchPipe, _sideIfBranchKw) <- P.try $ do
    let opt
          | isFirst = optional
          | otherwise = fmap Just
    pipe' <- Irrelevant <$> opt (kw kwPipe)
    condKw' <- ifElseKw
    return (pipe', condKw')
  _sideIfBranchCondition <- case sing :: SIfBranchKind k of
    SBranchIfBool -> parseExpressionAtoms
    SBranchIfElse -> return ()
  _sideIfBranchAssignKw <- Irrelevant <$> kw kwAssign
  _sideIfBranchBody <- parseExpressionAtoms
  return SideIfBranch {..}

sideIfs :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (SideIfs 'Parsed)
sideIfs = do
  fstBranch <- sideIfBranch True
  moreBranches <- many (sideIfBranch False)
  let _sideIfBranches = fstBranch :| moreBranches
  _sideIfElse <- optional (sideIfBranch False)
  return
    SideIfs
      { ..
      }

pcaseBranchRhs :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (CaseBranchRhs 'Parsed)
pcaseBranchRhs =
  CaseBranchRhsExpression <$> pcaseBranchRhsExpression
    <|> CaseBranchRhsIf <$> sideIfs

pcaseBranchRhsExpression :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RhsExpression 'Parsed)
pcaseBranchRhsExpression = do
  _rhsExpressionAssignKw <- Irrelevant <$> kw kwAssign
  _rhsExpression <- parseExpressionAtoms
  return RhsExpression {..}

caseBranch :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (CaseBranch 'Parsed)
caseBranch _caseBranchPipe = do
  _caseBranchPattern <- parsePatternAtoms
  _caseBranchRhs <- pcaseBranchRhs
  return CaseBranch {..}

case_ :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Case 'Parsed)
case_ = P.label "case" $ do
  _caseKw <- kw kwCase
  _caseExpression <- parseExpressionAtoms
  _caseOfKw <- kw kwOf
  _caseBranches <- braces (pipeSep1 caseBranch) <|> pipeSep1 caseBranch
  return Case {..}

ifBranch ::
  forall r k.
  (SingI k, Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (IfBranch 'Parsed k)
ifBranch = do
  _ifBranchPipe <- Irrelevant <$> pipeHelper
  _ifBranchCondition <- case sing :: SIfBranchKind k of
    SBranchIfBool -> parseExpressionAtoms
    SBranchIfElse -> Irrelevant <$> kw kwElse
  _ifBranchAssignKw <- Irrelevant <$> kw kwAssign
  _ifBranchExpression <- parseExpressionAtoms
  return IfBranch {..}
  where
    pipeHelper :: ParsecS r KeywordRef
    pipeHelper = case sing :: SIfBranchKind k of
      SBranchIfBool -> P.try (kw kwPipe <* P.notFollowedBy (kw kwElse))
      SBranchIfElse -> kw kwPipe

multiwayIf :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (If 'Parsed)
multiwayIf = do
  _ifKw <- kw kwIf
  _ifBranches <- many ifBranch
  _ifBranchElse <- ifBranch
  return If {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

universe :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r Universe
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

typeSig :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => SigOptions -> ParsecS r (TypeSig 'Parsed)
typeSig opts = do
  _typeSigArgs <- many (parseArg opts)
  _typeSigColonKw <-
    Irrelevant
      <$> if
          | opts ^. sigAllowOmitType -> optional (kw kwColon)
          | otherwise -> Just <$> kw kwColon
  _typeSigRetType <-
    case _typeSigColonKw ^. unIrrelevant of
      Just {} -> Just <$> parseExpressionAtoms
      Nothing -> return Nothing
  return TypeSig {..}

functionDefinitionLhs ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  FunctionSyntaxOptions ->
  Maybe (WithLoc BuiltinFunction) ->
  ParsecS r (FunctionLhs 'Parsed)
functionDefinitionLhs opts _funLhsBuiltin = P.label "<function definition>" $ do
  let allowInstance = opts ^. funAllowInstance
      allowOmitType = opts ^. funAllowOmitType
  _funLhsTerminating <- optional (kw kwTerminating)
  off <- P.getOffset
  _funLhsCoercion <- optional (kw kwCoercion)
  unless (allowInstance || isNothing _funLhsCoercion) $
    parseFailure off "coercion not allowed here"
  off0 <- P.getOffset
  _funLhsInstance <- optional (kw kwInstance)
  unless (allowInstance || isNothing _funLhsInstance) $
    parseFailure off0 "instance not allowed here"
  when (isJust _funLhsCoercion && isNothing _funLhsInstance) $
    parseFailure off0 "expected: instance"
  _funLhsName <- symbol
  let sigOpts =
        SigOptions
          { _sigAllowDefault = True,
            _sigAllowOmitType = allowOmitType
          }
  _funLhsTypeSig <- typeSig sigOpts
  return
    FunctionLhs
      { _funLhsInstance,
        _funLhsBuiltin,
        _funLhsCoercion,
        _funLhsName,
        _funLhsTypeSig,
        _funLhsTerminating
      }

parseArg :: forall r. (Members '[ParserResultBuilder, JudocStash, PragmasStash, Error ParserError] r) => SigOptions -> ParsecS r (SigArg 'Parsed)
parseArg opts = do
  (openDelim, _sigArgNames, _sigArgImplicit, _sigArgColon) <- P.try $ do
    (opn, impl) <- implicitOpen
    let parseArgumentName :: ParsecS r (Argument 'Parsed) =
          ArgumentSymbol <$> symbol
            <|> ArgumentWildcard <$> wildcard
    let parseArgumentNameColon :: ParsecS r (Argument 'Parsed, Irrelevant KeywordRef) = P.try $ do
          n <- parseArgumentName
          c <- Irrelevant <$> kw kwColon
          return (n, c)
    (ns :: SigArgNames 'Parsed, c) <- case impl of
      ImplicitInstance -> do
        ma <- optional parseArgumentNameColon
        return $ case ma of
          Just (ns', c') -> (SigArgNames (pure ns'), Just c')
          Nothing -> (SigArgNamesInstance, Nothing)
      Implicit -> do
        ns <- SigArgNames <$> some1 parseArgumentName
        c <- optional (Irrelevant <$> kw kwColon)
        return (ns, c)
      Explicit -> do
        ns <- SigArgNames <$> some1 parseArgumentName
        c <- Just . Irrelevant <$> kw kwColon
        return (ns, c)
    return (opn, ns, impl, c)
  _sigArgType <- case _sigArgImplicit of
    Implicit
      | isNothing _sigArgColon ->
          return Nothing
    _ ->
      Just <$> parseExpressionAtoms
  _sigArgDefault <-
    if
        | opts ^. sigAllowDefault -> optional $ do
            _argDefaultAssign <- Irrelevant <$> kw kwAssign
            _argDefaultValue <- parseExpressionAtoms
            return ArgDefault {..}
        | otherwise -> return Nothing
  closeDelim <- implicitClose _sigArgImplicit
  let _sigArgDelims = Irrelevant (openDelim, closeDelim)
  return SigArg {..}

functionDefinition ::
  forall r.
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  FunctionSyntaxOptions ->
  Maybe (WithLoc BuiltinFunction) ->
  ParsecS r (FunctionDef 'Parsed)
functionDefinition opts _signBuiltin = P.label "<function definition>" $ do
  FunctionLhs {..} <- functionDefinitionLhs opts _signBuiltin
  off <- P.getOffset
  _signDoc <- getJudoc
  _signPragmas <- getPragmas
  _signBody <- parseBody
  unless
    ( isJust (_funLhsTypeSig ^. typeSigColonKw . unIrrelevant)
        || (P.isBodyExpression _signBody && null (_funLhsTypeSig ^. typeSigArgs))
    )
    $ parseFailure off "expected result type"
  return
    FunctionDef
      { _signName = _funLhsName,
        _signTypeSig = _funLhsTypeSig,
        _signTerminating = _funLhsTerminating,
        _signInstance = _funLhsInstance,
        _signCoercion = _funLhsCoercion,
        _signBuiltin = _funLhsBuiltin,
        _signDoc,
        _signPragmas,
        _signBody
      }
  where
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
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
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

implicitOpenField ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  ParsecS r (KeywordRef, IsImplicitField)
implicitOpenField =
  (,ImplicitInstanceField) <$> kw delimDoubleBraceL
    <|> (,ExplicitField) <$> kw delimParenL

implicitOpen :: (Members '[ParserResultBuilder] r) => ParsecS r (KeywordRef, IsImplicit)
implicitOpen =
  (,ImplicitInstance) <$> kw delimDoubleBraceL
    <|> (,Implicit) <$> kw delimBraceL
    <|> (,Explicit) <$> kw delimParenL

implicitCloseField ::
  (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) =>
  IsImplicitField ->
  ParsecS r KeywordRef
implicitCloseField = \case
  ExplicitField -> kw delimParenR
  ImplicitInstanceField -> kw delimDoubleBraceR

implicitClose :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => IsImplicit -> ParsecS r KeywordRef
implicitClose = \case
  Implicit -> kw delimBraceR
  Explicit -> kw delimParenR
  ImplicitInstance -> kw delimDoubleBraceR

functionParams :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (FunctionParameters 'Parsed)
functionParams = P.label "<function type parameters>" $ do
  (openDelim, _paramNames, _paramImplicit, _paramColon) <- P.try $ do
    (opn, impl) <- implicitOpen
    -- checking that there is a : and not a := is needed to give a better error for missing @ in named application.
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

function :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Function 'Parsed)
function = do
  _funParameters <- functionParams
  _funKw <- kw kwRightArrow
  _funReturn <- parseExpressionAtoms
  return Function {..}

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Irrelevant (Maybe KeywordRef) -> ParsecS r (LambdaClause 'Parsed)
lambdaClause _lambdaPipe = do
  _lambdaParameters <- P.some patternAtom
  _lambdaAssignKw <- Irrelevant <$> kw kwAssign
  _lambdaBody <- parseExpressionAtoms
  return LambdaClause {..}

lambda :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Lambda 'Parsed)
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

inductiveDef :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Maybe (WithLoc BuiltinInductive) -> ParsecS r (InductiveDef 'Parsed)
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
  let name' = NameUnqualified _inductiveName
      params = fmap (AtomIdentifier . NameUnqualified) (concatMap (toList . (^. inductiveParametersNames)) _inductiveParameters)
      iden = mkExpressionAtoms (AtomIdentifier name' :| [])
      _inductiveTypeApplied = mkExpressionAtoms (AtomParens iden :| params)
  _inductiveConstructors <-
    pipeSep1 (constructorDef _inductiveName)
      P.<?> "<constructor definition>"
  return InductiveDef {..}

inductiveParamsLong :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
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

inductiveParamsShort :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParamsShort = do
  _inductiveParametersNames <- some1 symbol
  return
    InductiveParameters
      { _inductiveParametersRhs = Nothing,
        ..
      }

inductiveParams :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (InductiveParameters 'Parsed)
inductiveParams = inductiveParamsLong <|> inductiveParamsShort

rhsGadt :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RhsGadt 'Parsed)
rhsGadt = P.label "<constructor gadt>" $ do
  _rhsGadtColon <- Irrelevant <$> kw kwColon
  _rhsGadtType <- parseExpressionAtoms P.<?> "<constructor type>"
  return RhsGadt {..}

recordField :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RecordField 'Parsed)
recordField = do
  _fieldDoc <- optional stashJudoc >> getJudoc
  _fieldPragmas <- optional stashPragmas >> getPragmas
  _fieldBuiltin <- optional builtinRecordField
  mayImpl <- optional (snd <$> implicitOpenField)
  _fieldName <- symbol
  whenJust mayImpl (void . implicitCloseField)
  let _fieldIsImplicit = fromMaybe ExplicitField mayImpl
  _fieldColon <- Irrelevant <$> kw kwColon
  _fieldType <- parseExpressionAtoms
  return RecordField {..}

rhsAdt :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RhsAdt 'Parsed)
rhsAdt = P.label "<constructor arguments>" $ do
  _rhsAdtArguments <- many atomicExpression
  return RhsAdt {..}

rhsRecord :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RhsRecord 'Parsed)
rhsRecord = P.label "<constructor record>" $ do
  a <- optional (kw kwAt)
  l <- kw delimBraceL
  _rhsRecordStatements <- P.sepEndBy recordStatement semicolon
  r <- kw delimBraceR
  let _rhsRecordDelim = Irrelevant (a, l, r)
  return RhsRecord {..}

recordStatement :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RecordStatement 'Parsed)
recordStatement =
  RecordStatementSyntax <$> syntax
    <|> RecordStatementField <$> recordField
  where
    syntax :: ParsecS r (RecordSyntaxDef 'Parsed)
    syntax = do
      syn <- kw kwSyntax
      RecordSyntaxIterator <$> iteratorSyntaxDef syn
        <|> RecordSyntaxOperator <$> operatorSyntaxDef syn

pconstructorRhs :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ConstructorRhs 'Parsed)
pconstructorRhs =
  ConstructorRhsGadt <$> rhsGadt
    <|> ConstructorRhsRecord <$> rhsRecord
    <|> ConstructorRhsAdt <$> rhsAdt

constructorDef :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Symbol -> Irrelevant (Maybe KeywordRef) -> ParsecS r (ConstructorDef 'Parsed)
constructorDef _constructorInductiveName _constructorPipe = do
  _constructorDoc <- optional stashJudoc >> getJudoc
  _constructorPragmas <- optional stashPragmas >> getPragmas
  _constructorName <- symbol P.<?> "<constructor name>"
  _constructorRhs <- pconstructorRhs
  return ConstructorDef {..}

wildcard :: (Members '[ParserResultBuilder] r) => ParsecS r Wildcard
wildcard = Wildcard . snd <$> interval (kw kwWildcard)

patternAtomWildcardConstructor :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (WildcardConstructor 'Parsed)
patternAtomWildcardConstructor = P.try $ do
  _wildcardConstructor <- name
  _wildcardConstructorAtKw <- Irrelevant <$> kw kwAt
  l <- kw delimBraceL
  r <- kw delimBraceR
  let _wildcardConstructorDelims = Irrelevant (l, r)
  return WildcardConstructor {..}

-- | Used to report better errors when the user forgets the @ on a named
-- application. It tries to parse the lhs of a function definition (up to the
-- :=). If it succeeds, it reports an error.
checkNoNamedApplicationMissingAt :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r ()
checkNoNamedApplicationMissingAt = recoverStashes $ do
  let funSyntax =
        FunctionSyntaxOptions
          { _funAllowOmitType = True,
            _funAllowInstance = False
          }
  x <-
    P.observing
      . P.try
      . interval
      $ do
        fun <- symbol
        lbrace
        lhs <- functionDefinitionLhs funSyntax Nothing
        kw kwAssign
        return (fun, lhs)
  case x of
    Left {} -> return ()
    Right ((fun, lhs), loc) ->
      P.lift . throw $
        ErrNamedApplicationMissingAt
          NamedApplicationMissingAt
            { _namedApplicationMissingAtLoc = loc,
              _namedApplicationMissingAtLhs = lhs,
              _namedApplicationMissingAtFun = fun
            }

patternAtomAnon :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomAnon =
  PatternAtomWildcard <$> wildcard
    <|> PatternAtomDoubleBraces <$> doubleBraces parsePatternAtomsNested
    <|> PatternAtomParens <$> parens parsePatternAtomsNested
    <|> PatternAtomBraces <$> braces parsePatternAtomsNested
    <|> PatternAtomList <$> parseListPattern

patternAtomAt :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Symbol -> ParsecS r PatternBinding
patternAtomAt _patternBindingName = do
  _patternBindingAtKw <- Irrelevant <$> kw kwAt
  _patternBindingPattern <- patternAtom
  return PatternBinding {..}

recordPatternItem :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (RecordPatternItem 'Parsed)
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

patternAtomRecord :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Name -> ParsecS r (RecordPattern 'Parsed)
patternAtomRecord _recordPatternConstructor = do
  -- The try is needed to disambiguate from `at` pattern
  P.try (void (kw kwAt >> kw delimBraceL))
  _recordPatternItems <- P.sepEndBy recordPatternItem semicolon
  kw delimBraceR
  return
    RecordPattern {..}

-- | A pattern that starts with an identifier
patternAtomNamed :: forall r. (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
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

patternAtomNested :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtomNested = patternAtom' True

patternAtom :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (PatternAtom 'Parsed)
patternAtom = patternAtom' False

patternAtom' :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => Bool -> ParsecS r (PatternAtom 'Parsed)
patternAtom' nested =
  P.label "<pattern>" $
    PatternAtomWildcardConstructor <$> patternAtomWildcardConstructor
      <|> patternAtomNamed nested
      <|> patternAtomAnon

parsePatternAtoms :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtoms = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtom)
  return PatternAtoms {..}

parsePatternAtomsNested :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (PatternAtoms 'Parsed)
parsePatternAtomsNested = do
  (_patternAtoms, _patternAtomsLoc) <- second Irrelevant <$> interval (P.some patternAtomNested)
  return PatternAtoms {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmodulePath :: forall t r. (SingI t, Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ModulePathType 'Parsed t)
pmodulePath = case sing :: SModuleIsTop t of
  SModuleTop -> topModulePath
  SModuleLocal -> symbol

moduleDef :: forall t r. (SingI t, Members '[Error ParserError, ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (Module 'Parsed t)
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
atomicExpression :: (Members '[ParserResultBuilder, PragmasStash, Error ParserError, JudocStash] r) => ParsecS r (ExpressionAtoms 'Parsed)
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
         Error ParserError,
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

usingOrHiding :: (Members '[ParserResultBuilder, JudocStash, PragmasStash, Error ParserError] r) => ParsecS r (UsingHiding 'Parsed)
usingOrHiding =
  Using <$> pusingList
    <|> Hiding <$> phidingList
