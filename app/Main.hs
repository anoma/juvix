{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

--------------------------------------------------------------------------------

import Commands.Extra
import Commands.MicroJuvix
import Commands.MiniHaskell
import Commands.Termination as T
import Control.Exception qualified as IO
import Control.Monad.Extra
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Pretty hiding (Doc)
import MiniJuvix.Syntax.Abstract.Pretty.Ansi qualified as A
import MiniJuvix.Syntax.Concrete.Language qualified as M
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Highlight qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.Abstract.Pretty qualified as Abstract
import MiniJuvix.Syntax.MicroJuvix.Error qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.Pretty qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as MicroTyped
import MiniJuvix.Syntax.MiniHaskell.Pretty qualified as MiniHaskell
import MiniJuvix.Termination qualified as T
import MiniJuvix.Termination.CallGraph qualified as A
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as Micro
import MiniJuvix.Translation.MicroJuvixToMiniHaskell qualified as MiniHaskell
import MiniJuvix.Translation.ScopedToAbstract qualified as Abstract
import MiniJuvix.Utils.Version (runDisplayVersion)
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Console.ANSI qualified as Ansi
import System.IO qualified as IO
import Text.Show.Pretty hiding (Html)

--------------------------------------------------------------------------------

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool
  }

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

data CLI = CLI
  { _cliGlobalOptions :: GlobalOptions,
    _cliCommand :: Command
  }

data ScopeOptions = ScopeOptions
  { _scopeInputFiles :: NonEmpty FilePath,
    _scopeShowIds :: Bool,
    _scopeInlineImports :: Bool
  }

data ParseOptions = ParseOptions
  { _parseInputFile :: FilePath,
    _parseNoPrettyShow :: Bool
  }

newtype HighlightOptions = HighlightOptions
  { _highlightInputFile :: FilePath
  }

data HtmlOptions = HtmlOptions
  { _htmlInputFile :: FilePath,
    _htmlRecursive :: Bool,
    _htmlTheme :: Theme
  }

makeLenses ''GlobalOptions
makeLenses ''CLI

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable globally ANSI formatting "
      )
  pure GlobalOptions {..}

parseCLI :: Parser CLI
parseCLI = do
  _cliGlobalOptions <- parseGlobalOptions
  _cliCommand <- parseCommand
  pure CLI {..}

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
  _scopeInputFiles <-
    some1 $
      argument
        str
        ( metavar "MINIJUVIX_FILE(s)"
            <> help "Path to one ore more MiniJuvix files"
            <> action "file"
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

descr :: ParserInfo CLI
descr =
  info
    (parseCLI <**> helper)
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
    <|> hsubparser
      ( mconcat
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

mkScopePrettyOptions :: ScopeOptions -> Scoper.Options
mkScopePrettyOptions ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameId = _scopeShowIds,
      Scoper._optInlineImports = _scopeInlineImports
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

class HasEntryPoint a where
  getEntryPoint :: FilePath -> a -> EntryPoint

instance HasEntryPoint ScopeOptions where
  getEntryPoint root = EntryPoint root . _scopeInputFiles

instance HasEntryPoint ParseOptions where
  getEntryPoint root = EntryPoint root . pure . _parseInputFile

instance HasEntryPoint HighlightOptions where
  getEntryPoint root = EntryPoint root . pure . _highlightInputFile

instance HasEntryPoint HtmlOptions where
  getEntryPoint root = EntryPoint root . pure . _htmlInputFile

instance HasEntryPoint MicroJuvixOptions where
  getEntryPoint root = EntryPoint root . pure . _mjuvixInputFile

instance HasEntryPoint MiniHaskellOptions where
  getEntryPoint root = EntryPoint root . pure . _mhaskellInputFile

instance HasEntryPoint CallsOptions where
  getEntryPoint root = EntryPoint root . pure . _callsInputFile

instance HasEntryPoint CallGraphOptions where
  getEntryPoint root = EntryPoint root . pure . _graphInputFile

runCLI :: CLI -> IO ()
runCLI cli = do
  let useColors = not (cli ^. (cliGlobalOptions . globalNoColors))
      renderIO' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> IO ()
      renderIO' = renderIO useColors
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText useColors
  root <- findRoot
  case cli ^. cliCommand of
    DisplayVersion -> runDisplayVersion
    DisplayRoot -> putStrLn (pack root)
    Scope opts -> do
      l <- (^. Scoper.resultModules) <$> runIO (upToScoping (getEntryPoint root opts))
      forM_ l $ \s -> do
        renderIO' (Scoper.ppOut' (mkScopePrettyOptions opts) s)
    Highlight o -> do
      let entry :: EntryPoint
          entry = getEntryPoint root o
      res <- runIO (upToScoping entry)
      let tbl = res ^. Scoper.resultParserTable
          items = tbl ^. Parser.infoParsedItems
          names = res ^. (Scoper.resultScoperTable . Scoper.infoNames)
      putStrLn (Scoper.go items names)
    Parse ParseOptions {..} -> do
      m <- parseModuleIO _parseInputFile
      if _parseNoPrettyShow then print m else pPrint m
    Html o@HtmlOptions {..} -> do
      res <- runIO (upToScoping (getEntryPoint root o))
      let m = head (res ^. Scoper.resultModules)
      genHtml Scoper.defaultOptions _htmlRecursive _htmlTheme m
    MicroJuvix (Pretty opts) -> do
      micro <- head . (^. Micro.resultModules) <$> runIO (upToMicroJuvix (getEntryPoint root opts))
      renderIO' (Micro.ppOut micro)
    MicroJuvix (TypeCheck opts) -> do
      micro <- head . (^. MicroTyped.resultModules) <$> runIO (upToMicroJuvixTyped (getEntryPoint root opts))
      case MicroTyped.checkModule micro of
        Right _ -> putStrLn "Well done! It type checks"
        Left (Micro.TypeCheckerErrors es) -> sequence_ (intersperse (putStrLn "")
                        (printErrorAnsi <$> toList es)) >> exitFailure
    MiniHaskell o -> do
      minihaskell <- head . (^. MiniHaskell.resultModules) <$> runIO (upToMiniHaskell (getEntryPoint root o))
      supportsAnsi <- Ansi.hSupportsANSI IO.stdout
      -- TODO fix #38
      renderIO (supportsAnsi && useColors) (MiniHaskell.ppOut minihaskell)
    Termination (Calls opts@CallsOptions {..}) -> do
      a <- head . (^. Abstract.resultModules) <$> runIO (upToAbstract (getEntryPoint root opts))
      let callMap0 = T.buildCallMap a
          callMap = case _callsFunctionNameFilter of
            Nothing -> callMap0
            Just f -> T.filterCallMap f callMap0
          opts' = T.callsPrettyOptions opts
      A.printPrettyCode opts' callMap
      putStrLn ""
    Termination (CallGraph o@CallGraphOptions {..}) -> do
      a <- head . (^. Abstract.resultModules) <$> runIO (upToAbstract (getEntryPoint root o))
      let callMap = T.buildCallMap a
          opts' = A.defaultOptions
          completeGraph = T.completeCallGraph callMap
          filteredGraph = maybe completeGraph (`T.unsafeFilterGraph` completeGraph) _graphFunctionNameFilter
          recBehav = map T.recursiveBehaviour (T.reflexiveEdges filteredGraph)
      A.printPrettyCode opts' filteredGraph
      putStrLn ""
      forM_ recBehav $ \r -> do
        let n = toAnsiText' (Scoper.ppOut (A._recBehaviourFunction r))
        renderIO useColors (Abstract.ppOut r)
        putStrLn ""
        case T.findOrder r of
          Nothing -> putStrLn (n <> " Fails the termination checking")
          Just (T.LexOrder k) -> putStrLn (n <> " Terminates with order " <> show (toList k))
        putStrLn ""

main :: IO ()
main = execParser descr >>= runCLI
