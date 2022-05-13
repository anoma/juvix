{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Commands.Extra
import Commands.Html
import Commands.MicroJuvix
import Commands.MiniC
import Commands.MiniHaskell
import Commands.MonoJuvix
import Commands.Parse
import Commands.Scope
import Commands.Termination as Termination
import Control.Exception qualified as IO
import Control.Monad.Extra
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Pretty hiding (Doc)
import MiniJuvix.Syntax.Abstract.InfoTable qualified as Abstract
import MiniJuvix.Syntax.Abstract.Language qualified as Abstract
import MiniJuvix.Syntax.Abstract.Pretty qualified as Abstract
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Highlight qualified as Highlight
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.MicroJuvix.Pretty qualified as Micro
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as MicroTyped
import MiniJuvix.Syntax.MiniHaskell.Pretty qualified as MiniHaskell
import MiniJuvix.Syntax.MonoJuvix.Pretty qualified as Mono
import MiniJuvix.Termination qualified as Termination
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as Micro
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC qualified as MiniC
import MiniJuvix.Translation.MonoJuvixToMiniHaskell qualified as MiniHaskell
import MiniJuvix.Translation.ScopedToAbstract qualified as Abstract
import MiniJuvix.Utils.Version (runDisplayVersion)
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Console.ANSI qualified as Ansi
import System.IO qualified as IO
import Text.Show.Pretty hiding (Html)

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool
  }

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions
  | Termination TerminationCommand
  | MiniHaskell MiniHaskellOptions
  | MiniC MiniCOptions
  | MicroJuvix MicroJuvixCommand
  | MonoJuvix MonoJuvixOptions
  | DisplayVersion
  | DisplayRoot
  | Highlight HighlightOptions

data CLI = CLI
  { _cliGlobalOptions :: GlobalOptions,
    _cliCommand :: Command
  }

newtype HighlightOptions = HighlightOptions
  { _highlightInputFile :: FilePath
  }

makeLenses ''HighlightOptions
makeLenses ''GlobalOptions
makeLenses ''CLI

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable globally ANSI formatting"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  pure GlobalOptions {..}

parseCLI :: Parser CLI
parseCLI = do
  _cliGlobalOptions <- parseGlobalOptions
  _cliCommand <- parseCommand
  pure CLI {..}

parseHighlight :: Parser HighlightOptions
parseHighlight = do
  _highlightInputFile <- parserInputFile
  pure HighlightOptions {..}

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
            commandMonoJuvix,
            commandMicroJuvix,
            commandMiniHaskell,
            commandMiniC,
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

    commandMonoJuvix :: Mod CommandFields Command
    commandMonoJuvix = command "monojuvix" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MonoJuvix <$> parseMonoJuvix)
            (progDesc "Translate a MiniJuvix file to MonoJuvix")

    commandMiniHaskell :: Mod CommandFields Command
    commandMiniHaskell = command "minihaskell" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MiniHaskell <$> parseMiniHaskell)
            (progDesc "Translate a MiniJuvix file to MiniHaskell")

    commandMiniC :: Mod CommandFields Command
    commandMiniC = command "minic" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (MiniC <$> parseMiniC)
            (progDesc "Translate a MiniJuvix file to MiniC")

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

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameId = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }

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
  getEntryPoint root = EntryPoint root . (^. scopeInputFiles)

instance HasEntryPoint ParseOptions where
  getEntryPoint root = EntryPoint root . pure . (^. parseInputFile)

instance HasEntryPoint HighlightOptions where
  getEntryPoint root = EntryPoint root . pure . (^. highlightInputFile)

instance HasEntryPoint HtmlOptions where
  getEntryPoint root = EntryPoint root . pure . (^. htmlInputFile)

instance HasEntryPoint MicroJuvixTypeOptions where
  getEntryPoint root = EntryPoint root . pure . (^. microJuvixTypeInputFile)

instance HasEntryPoint MicroJuvixPrettyOptions where
  getEntryPoint root = EntryPoint root . pure . (^. microJuvixPrettyInputFile)

instance HasEntryPoint MonoJuvixOptions where
  getEntryPoint root = EntryPoint root . pure . (^. monoJuvixInputFile)

instance HasEntryPoint MiniHaskellOptions where
  getEntryPoint root = EntryPoint root . pure . (^. miniHaskellInputFile)

instance HasEntryPoint MiniCOptions where
  getEntryPoint root = EntryPoint root . pure . (^. miniCInputFile)

instance HasEntryPoint CallsOptions where
  getEntryPoint root = EntryPoint root . pure . (^. callsInputFile)

instance HasEntryPoint CallGraphOptions where
  getEntryPoint root = EntryPoint root . pure . (^. graphInputFile)

runCLI :: CLI -> IO ()
runCLI cli = do
  supportsAnsiStdOut <- Ansi.hSupportsANSI IO.stdout
  let globalOptions = cli ^. cliGlobalOptions
      useColors = not (globalOptions ^. globalNoColors)
      toAnsiText' :: forall a. (HasAnsiBackend a, HasTextBackend a) => a -> Text
      toAnsiText' = toAnsiText useColors
      -- Note: Probably a GHC bug specialises renderStdOut to the type
      -- of the arugment for its first usage in the rest of the do block .
      renderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> IO ()
      renderStdOut = renderIO (supportsAnsiStdOut && useColors)
      renderStdOutAbs :: Abstract.PPOutput -> IO ()
      renderStdOutAbs = renderStdOut
      renderStdOutMini :: MiniHaskell.PPOutput -> IO ()
      renderStdOutMini = renderStdOut
      renderStdOutMicro :: Micro.PPOutput -> IO ()
      renderStdOutMicro = renderStdOut
      renderStdOutMono :: Mono.PPOutput -> IO ()
      renderStdOutMono = renderStdOut
  root <- findRoot
  case cli ^. cliCommand of
    DisplayVersion -> runDisplayVersion
    DisplayRoot -> putStrLn (pack root)
    Highlight o -> do
      let entry :: EntryPoint
          entry = getEntryPoint root o
      res <- runIO (upToScoping entry)
      absP <- makeAbsolute (o ^. highlightInputFile)
      let tbl = res ^. Scoper.resultParserTable
          items = tbl ^. Parser.infoParsedItems
          names = res ^. (Scoper.resultScoperTable . Scoper.infoNames)
          hinput =
            Highlight.filterInput
              absP
              Highlight.HighlightInput
                { _highlightNames = names,
                  _highlightParsed = items
                }
      putStrLn (Highlight.go hinput)
    Parse opts -> do
      m <- head . (^. Parser.resultModules) <$> runIO (upToParsing (getEntryPoint root opts))
      if opts ^. parseNoPrettyShow then print m else pPrint m
    Scope opts -> do
      l <- (^. Scoper.resultModules) <$> runIO (upToScoping (getEntryPoint root opts))
      forM_ l $ \s -> do
        renderStdOut (Scoper.ppOut (mkScopePrettyOptions globalOptions opts) s)
    Html o@HtmlOptions {..} -> do
      res <- runIO (upToScoping (getEntryPoint root o))
      let m = head (res ^. Scoper.resultModules)
      genHtml Scoper.defaultOptions _htmlRecursive _htmlTheme m
    MicroJuvix (Pretty opts) -> do
      micro <- head . (^. Micro.resultModules) <$> runIO (upToMicroJuvix (getEntryPoint root opts))
      let ppOpts =
            Micro.defaultOptions
              { Micro._optShowNameId = globalOptions ^. globalShowNameIds
              }
      renderStdOutMicro (Micro.ppOut ppOpts micro)
    MicroJuvix (TypeCheck opts) -> do
      micro <- runIOEither (upToMicroJuvixTyped (getEntryPoint root opts))
      case micro of
        Right res -> do
          putStrLn "Well done! It type checks"
          when (opts ^. microJuvixTypePrint) $ do
            let ppOpts =
                  Micro.defaultOptions
                    { Micro._optShowNameId = globalOptions ^. globalShowNameIds
                    }
                checkedModule = head (res ^. MicroTyped.resultModules)
            renderStdOutMicro (Micro.ppOut ppOpts checkedModule)
            putStrLn ""
            let typeCalls = Mono.buildTypeCallMap res
            renderStdOutMicro (Micro.ppOut ppOpts typeCalls)
            putStrLn ""
            let concreteTypeCalls = Mono.collectTypeCalls res
            renderStdOutMicro (Micro.ppOut ppOpts concreteTypeCalls)
        Left err -> printErrorAnsiSafe err >> exitFailure
    MonoJuvix o -> do
      let ppOpts =
            Mono.defaultOptions
              { Mono._optShowNameIds = globalOptions ^. globalShowNameIds
              }
      monojuvix <- head . (^. Mono.resultModules) <$> runIO (upToMonoJuvix (getEntryPoint root o))
      renderStdOutMono (Mono.ppOut ppOpts monojuvix)
    MiniHaskell o -> do
      minihaskell <- head . (^. MiniHaskell.resultModules) <$> runIO (upToMiniHaskell (getEntryPoint root o))
      renderStdOutMini (MiniHaskell.ppOutDefault minihaskell)
    MiniC o -> do
      miniC <- (^. MiniC.resultCCode) <$> runIO (upToMiniC (getEntryPoint root o))
      putStrLn miniC
    Termination (Calls opts@CallsOptions {..}) -> do
      results <- runIO (upToAbstract (getEntryPoint root opts))
      let topModule = head (results ^. Abstract.resultModules)
          infotable = results ^. Abstract.resultTable
          callMap0 = Termination.buildCallMap infotable topModule
          callMap = case _callsFunctionNameFilter of
            Nothing -> callMap0
            Just f -> Termination.filterCallMap f callMap0
          opts' = Termination.callsPrettyOptions opts
      renderStdOutAbs (Abstract.ppOut opts' callMap)
      putStrLn ""
    Termination (CallGraph opts@CallGraphOptions {..}) -> do
      results <- runIO (upToAbstract (getEntryPoint root opts))
      let topModule = head (results ^. Abstract.resultModules)
          infotable = results ^. Abstract.resultTable
          callMap = Termination.buildCallMap infotable topModule
          opts' =
            Abstract.defaultOptions
              { Abstract._optShowNameId = globalOptions ^. globalShowNameIds
              }
          completeGraph = Termination.completeCallGraph callMap
          filteredGraph = maybe completeGraph (`Termination.unsafeFilterGraph` completeGraph) _graphFunctionNameFilter
          rEdges = Termination.reflexiveEdges filteredGraph
          recBehav = map Termination.recursiveBehaviour rEdges
      renderStdOutAbs (Abstract.ppOut opts' filteredGraph)
      putStrLn ""
      forM_ recBehav $ \r -> do
        let funName = r ^. Termination.recursiveBehaviourFun
            funRef = Abstract.FunctionRef (Scoper.unqualifiedSymbol funName)
            funInfo = HashMap.lookupDefault impossible funRef (infotable ^. Abstract.infoFunctions)
            markedTerminating = funInfo ^. (Abstract.functionInfoDef . Abstract.funDefTerminating)
            sopts =
              Scoper.defaultOptions
                { Scoper._optShowNameId = globalOptions ^. globalShowNameIds
                }
            n = toAnsiText' (Scoper.ppOut sopts funName)
        renderStdOutAbs (Abstract.ppOut opts' r)
        putStrLn ""
        if
            | markedTerminating -> putStrLn (n <> " Terminates by assumption ")
            | otherwise ->
                case Termination.findOrder r of
                  Nothing -> putStrLn (n <> " Fails the termination checking") >> exitFailure
                  Just (Termination.LexOrder k) -> putStrLn (n <> " Terminates with order " <> show (toList k))
        putStrLn ""

main :: IO ()
main = execParser descr >>= runCLI
