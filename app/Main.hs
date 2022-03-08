{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Control.Monad.Extra
import qualified MiniJuvix.Syntax.Concrete.Language as M
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as M
import qualified MiniJuvix.Termination as T
import qualified MiniJuvix.Translation.ScopedToAbstract as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as M
import qualified MiniJuvix.Termination.CallGraph as A
import qualified MiniJuvix.Syntax.Abstract.Pretty.Base as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Help.Pretty
import Text.Show.Pretty hiding (Html)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (defaultOptions)
import qualified MiniJuvix.Syntax.Abstract.Pretty.Ansi as A
import Commands.Extra
import Commands.Termination as T

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions
  | Termination TerminationCommand

data ScopeOptions = ScopeOptions
  { _scopeRootDir :: FilePath,
    _scopeInputFiles :: [FilePath],
    _scopeShowIds :: Bool,
    _scopeInlineImports :: Bool
  }

data ParseOptions = ParseOptions
  { _parseInputFile :: FilePath,
    _parseNoPrettyShow :: Bool
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
  _htmlTheme <- option (eitherReader parseTheme)
      ( long "theme"
          <> metavar "THEME"
          <> value Nord
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
    some $ argument
      str
      ( metavar "MINIJUVIX_FILE(s)"
          <> help "Path to one ore more .mjuvix files"
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
  pure ScopeOptions {..}

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
    foot = bold "maintainers: " <> "jan@heliax.dev; jonathan@heliax.dev"

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ commandParse,
        commandScope,
        commandHtml,
        commandTermination
      ]
  where
    commandParse :: Mod CommandFields Command
    commandParse = command "parse" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Parse <$> parseParse)
            (progDesc "Parse a .mjuvix file")

    commandHtml :: Mod CommandFields Command
    commandHtml = command "html" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Html <$> parseHtml)
            (progDesc "Generate html for a .mjuvix file")
    commandScope :: Mod CommandFields Command
    commandScope = command "scope" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (Scope <$> parseScope)
            (progDesc "Parse and scope a .mjuvix file")
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
parseModuleIO = fromRightIO id . M.runModuleParserIO

fromRightIO' :: (e -> IO ()) -> IO (Either e r) -> IO r
fromRightIO' pp = do
  eitherM ifLeft return
  where
  ifLeft e = pp e >> exitFailure

fromRightIO :: (e -> Text) -> IO (Either e r) -> IO r
fromRightIO pp = fromRightIO' (putStrLn . pp)

go :: Command -> IO ()
go c = do
  root <- getCurrentDirectory
  case c of
    Scope opts@ScopeOptions {..} -> do
      forM_ _scopeInputFiles $ \scopeInputFile -> do
        m <- parseModuleIO scopeInputFile
        s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
        M.printPrettyCode (mkScopePrettyOptions opts) s
    Parse ParseOptions {..} -> do
      m <- parseModuleIO _parseInputFile
      if _parseNoPrettyShow then print m else pPrint m
    Html HtmlOptions {..} -> do
      m <- parseModuleIO _htmlInputFile
      s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
      genHtml defaultOptions _htmlRecursive _htmlTheme s
    Termination (Calls opts@CallsOptions {..}) -> do
      m <- parseModuleIO _callsInputFile
      s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
      a <- fromRightIO' putStrLn (return $ A.translateModule s)
      let callMap0 =  T.buildCallMap a
          callMap = case _callsFunctionNameFilter of
            Nothing -> callMap0
            Just f -> T.filterCallMap f callMap0
          opts' = T.callsPrettyOptions opts
      A.printPrettyCode opts' callMap
      putStrLn ""
    Termination (CallGraph CallGraphOptions {..}) -> do
      m <- parseModuleIO _graphInputFile
      s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
      a <- fromRightIO' putStrLn (return $ A.translateModule s)
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

main :: IO ()
main = execParser descr >>= go
