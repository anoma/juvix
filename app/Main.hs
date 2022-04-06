{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

--------------------------------------------------------------------------------

import Commands.Extra
import Commands.MicroJuvix
import Commands.MiniHaskell
import Commands.Termination as T
import Control.Monad.Extra
import qualified Control.Exception as IO
import MiniJuvix.Prelude hiding (Doc)
import qualified MiniJuvix.Syntax.Abstract.Pretty.Ansi as A
import qualified MiniJuvix.Syntax.Concrete.Language as M
import qualified MiniJuvix.Syntax.Concrete.Parser as Parser
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.InfoTable as Scoped
import qualified MiniJuvix.Syntax.Concrete.Scoped.Highlight as Scoped
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (defaultOptions)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as M
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text as T
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as Scoper
import MiniJuvix.Pipeline
import qualified MiniJuvix.Syntax.MicroJuvix.Pretty.Ansi as Micro
import qualified MiniJuvix.Syntax.MicroJuvix.TypeChecker as Micro
import qualified MiniJuvix.Syntax.MicroJuvix.Language as Micro
import qualified MiniJuvix.Termination as T
import qualified MiniJuvix.Termination.CallGraph as A
import qualified MiniJuvix.Translation.AbstractToMicroJuvix as Micro
import qualified MiniJuvix.Translation.ScopedToAbstract as A
import qualified MiniJuvix.Translation.MicroJuvixToMiniHaskell as Hask
import qualified MiniJuvix.Syntax.MiniHaskell.Pretty.Ansi as Hask
import MiniJuvix.Utils.Version (runDisplayVersion)
import Options.Applicative
import Options.Applicative.Help.Pretty
import Text.Show.Pretty hiding (Html)
import MiniJuvix.Syntax.Concrete.Parser.InfoTable (infoParsedItems)

--------------------------------------------------------------------------------

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions
  | Termination TerminationCommand
  | MiniHaskell MiniHaskellOptions
  | MicroJuvix MicroJuvixCommand
  | DisplayVersion
  | DisplayRoot
  | Highlight HighlightOptions

data ScopeOptions = ScopeOptions
  { _scopeRootDir :: FilePath,
    _scopeInputFiles :: [FilePath],
    _scopeShowIds :: Bool,
    _scopeInlineImports :: Bool,
    _scopeNoColors :: Bool
  }

data ParseOptions = ParseOptions
  { _parseInputFile :: FilePath,
    _parseNoPrettyShow :: Bool
  }

data HighlightOptions = HighlightOptions
  { _highlightInputFile :: FilePath
  }

data HtmlOptions = HtmlOptions
  { _htmlInputFile :: FilePath,
    _htmlRecursive :: Bool,
    _htmlTheme :: Theme
  }

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlInputFile <- parseInputFile
  _htmlRecursive <-
    switch
      ( long "recursive"
          <> help "export imported modules recursively"
      )
  _htmlTheme <-
    option
      (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Ayu
          <> showDefault
          <> help "selects a theme: ayu (light); nord (dark)"
      )
  pure HtmlOptions {..}
  where
    parseTheme :: String -> Either String Theme
    parseTheme s = case s of
      "nord" -> Right Nord
      "ayu" -> Right Ayu
      _ -> Left $ "unrecognised theme: " <> s

parseHighlight :: Parser HighlightOptions
parseHighlight = do
  _highlightInputFile <- parseInputFile
  pure HighlightOptions {..}

parseParse :: Parser ParseOptions
parseParse = do
  _parseInputFile <- parseInputFile
  _parseNoPrettyShow <-
    switch
      ( long "no-pretty-show"
          <> help "Disable formatting of the Haskell AST"
      )
  pure ParseOptions {..}

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeRootDir <-
    strOption
      ( long "rootDir"
          <> short 'd'
          <> metavar "DIR"
          <> value "."
          <> showDefault
          <> help "Root directory"
      )
  _scopeInputFiles <-
    some $
      argument
        str
        ( metavar "MINIJUVIX_FILE(s)"
            <> help "Path to one ore more MiniJuvix files"
        )
  _scopeShowIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier"
      )
  _scopeInlineImports <-
    switch
      ( long "inline-imports"
          <> help "Show the code of imported modules next to the import statement"
      )
  _scopeNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
      )
  pure ScopeOptions {..}

parseDisplayVersion :: Parser Command
parseDisplayVersion =
  flag'
    DisplayVersion
    (long "version" <> short 'v' <> help "Print the version and exit")

parseDisplayRoot :: Parser Command
parseDisplayRoot =
  flag'
    DisplayRoot
    (long "show-root" <> help "Print the detected root of the project")


descr :: ParserInfo Command
descr =
  info
    (parseCommand <**> helper)
    ( fullDesc
        <> progDesc "The MiniJuvix compiler."
        <> headerDoc (Just headDoc)
        <> footerDoc (Just foot)
    )
  where
    headDoc :: Doc
    headDoc = dullblue $ bold $ underline "MiniJuvix help"

    foot :: Doc
    foot = bold "maintainers: " <> "The MiniJuvix Team"

parseCommand :: Parser Command
parseCommand =
  parseDisplayVersion
  <|> parseDisplayRoot
  <|> ( hsubparser $
          mconcat
            [ commandParse,
              commandScope,
              commandHtml,
              commandTermination,
              commandMicroJuvix,
              commandMiniHaskell,
              commandHighlight
            ]
      )
  where
    commandMicroJuvix :: Mod CommandFields Command
    commandMicroJuvix = command "microjuvix" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MicroJuvix <$> parseMicroJuvixCommand)
            (progDesc "Subcommands related to MicroJuvix")

    commandMiniHaskell :: Mod CommandFields Command
    commandMiniHaskell = command "minihaskell" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MiniHaskell <$> parseMiniHaskell)
            (progDesc "Translate a MiniJuvix file to MiniHaskell")

    commandHighlight :: Mod CommandFields Command
    commandHighlight = command "highlight" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Highlight <$> parseHighlight)
            (progDesc "Highlight a MiniJuvix file")

    commandParse :: Mod CommandFields Command
    commandParse = command "parse" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Parse <$> parseParse)
            (progDesc "Parse a MiniJuvix file")

    commandHtml :: Mod CommandFields Command
    commandHtml = command "html" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Html <$> parseHtml)
            (progDesc "Generate HTML for a MiniJuvix file")

    commandScope :: Mod CommandFields Command
    commandScope = command "scope" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Scope <$> parseScope)
            (progDesc "Parse and scope a MiniJuvix file")

    commandTermination :: Mod CommandFields Command
    commandTermination = command "termination" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Termination <$> parseTerminationCommand)
            (progDesc "Subcommands related to termination checking")

mkScopePrettyOptions :: ScopeOptions -> M.Options
mkScopePrettyOptions ScopeOptions {..} =
  M.defaultOptions
    { M._optShowNameId = _scopeShowIds,
      M._optInlineImports = _scopeInlineImports
    }

parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseModuleIO = fromRightIO id . Parser.runModuleParserIO

-- parseModuleIO' :: FilePath -> IO Parser.ParserResult
-- parseModuleIO' = fromRightIO id . Parser.runModuleParserIO'

minijuvixYamlFile :: FilePath
minijuvixYamlFile = "minijuvix.yaml"

findRoot :: IO FilePath
findRoot = do
  r <- IO.try go :: IO (Either IO.SomeException FilePath)
  case r of
    Left err -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      cur <- getCurrentDirectory
      putStrLn ("I will try to use the current directory: " <> pack cur)
      return cur
    Right root -> return root
  where
  possiblePaths :: FilePath -> [FilePath]
  possiblePaths start = takeWhile (/= "/") (aux start)
    where
    aux f = f : aux (takeDirectory f)
  go :: IO FilePath
  go = do
    c <- getCurrentDirectory
    l <- findFile (possiblePaths c) minijuvixYamlFile
    case l of
      Nothing -> return c
      Just yaml -> return (takeDirectory yaml)

runCommand :: Command -> IO ()
runCommand c = do
  root <- findRoot
  case c of
    DisplayVersion -> runDisplayVersion
    DisplayRoot -> putStrLn (pack root)
    Scope opts@ScopeOptions {..} -> do
      forM_ _scopeInputFiles $ \scopeInputFile -> do
        m <- parseModuleIO scopeInputFile
        s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
        printer (mkScopePrettyOptions opts) s
      where
        printer :: M.Options -> M.Module 'M.Scoped 'M.ModuleTop -> IO ()
        printer
          | not _scopeNoColors = M.printPrettyCode
          | otherwise = T.printPrettyCode
    Highlight HighlightOptions {..} -> do
      let
        entryp :: EntryPoint
        entryp = EntryPoint {
          _entryRoot = root,
          _entryModulePaths = pure _highlightInputFile
          }
      parserRes <- parseModuleIO' _highlightInputFile
      let parsedMod = head (parserRes ^. Parser.resultModules)
      res <- fromRightIO' printErrorAnsi $ Scoper.scopeCheck1IO root parsedMod
      let
        parserTable = parserRes ^. Parser.resultTable
        names = res ^. Scoper.resultScoperTable . Scoped.infoNames
        parsedItems = res ^. Scoper.resultParserTable . Parser.infoParsedItems
          <> parserTable ^. infoParsedItems
      putStrLn (Scoped.go parsedItems names)
    Parse ParseOptions {..} -> do
      m <- parseModuleIO _parseInputFile
      if _parseNoPrettyShow then print m else pPrint m
    Html HtmlOptions {..} -> do
      m <- parseModuleIO _htmlInputFile
      s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
      genHtml defaultOptions _htmlRecursive _htmlTheme s
    MicroJuvix (Pretty MicroJuvixOptions {..}) -> do
      micro <- miniToMicro root _mjuvixInputFile
      Micro.printPrettyCodeDefault micro
    MicroJuvix (TypeCheck MicroJuvixOptions {..}) -> do
      micro <- miniToMicro root _mjuvixInputFile
      case Micro.checkModule micro of
        Right _ -> putStrLn "Well done! It type checks"
        Left es -> sequence_ (intersperse (putStrLn "") (printErrorAnsi <$> toList es)) >> exitFailure
    MiniHaskell MiniHaskellOptions {..} -> do
      m <- parseModuleIO _mhaskellInputFile
      s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
      (_, a) <- fromRightIO' putStrLn (return (A.translateModule s))
      let micro = Micro.translateModule a
      case Micro.checkModule micro of
        Right checkedMicro -> do
          minihaskell <- fromRightIO' putStrLn (return $ Hask.translateModule checkedMicro)
          Hask.printPrettyCodeDefault minihaskell
        Left es -> mapM_ printErrorAnsi es >> exitFailure
    Termination (Calls opts@CallsOptions {..}) -> do
      m <- parseModuleIO _callsInputFile
      s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
      (_, a) <- fromRightIO' putStrLn (return $ A.translateModule s)
      let callMap0 = T.buildCallMap a
          callMap = case _callsFunctionNameFilter of
            Nothing -> callMap0
            Just f -> T.filterCallMap f callMap0
          opts' = T.callsPrettyOptions opts
      A.printPrettyCode opts' callMap
      putStrLn ""
    Termination (CallGraph CallGraphOptions {..}) -> do
      m <- parseModuleIO _graphInputFile
      s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
      (_, a) <- fromRightIO' putStrLn (return (A.translateModule s))
      let callMap = T.buildCallMap a
          opts' = A.defaultOptions
          completeGraph = T.completeCallGraph callMap
          filteredGraph = maybe completeGraph (`T.unsafeFilterGraph` completeGraph) _graphFunctionNameFilter
          recBehav = map T.recursiveBehaviour (T.reflexiveEdges filteredGraph)
      A.printPrettyCode opts' filteredGraph
      putStrLn ""
      forM_ recBehav $ \r -> do
        let n = M.renderPrettyCode M.defaultOptions $ A._recBehaviourFunction r
        A.printPrettyCode A.defaultOptions r
        putStrLn ""
        case T.findOrder r of
          Nothing -> putStrLn (n <> " Fails the termination checking")
          Just (T.LexOrder k) -> putStrLn (n <> " Terminates with order " <> show (toList k))
        putStrLn ""
    where
      miniToMicro :: FilePath -> FilePath -> IO Micro.Module
      miniToMicro root p = do
        m <- parseModuleIO p
        s <- head . Scoper._resultModules <$> fromRightIO' printErrorAnsi (Scoper.scopeCheck1IO root m)
        (_, a) <- fromRightIO' putStrLn (return $ A.translateModule s)
        return (Micro.translateModule a)

main :: IO ()
main = execParser descr >>= runCommand
