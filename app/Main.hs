{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Monad.Extra
import qualified MiniJuvix.Syntax.Concrete.Language as M
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as M
import qualified MiniJuvix.Termination.CallGraph as A
import qualified MiniJuvix.Translation.ScopedToAbstract as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as M
import qualified MiniJuvix.Syntax.Abstract.Pretty.Base as A
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Help.Pretty
import Text.Show.Pretty hiding (Html)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (defaultOptions)
import qualified MiniJuvix.Syntax.Abstract.Pretty.Ansi as A

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions
  | CallGraph CallGraphOptions

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

data CallGraphOptions = CallGraphOptions
  { _graphInputFile :: FilePath,
    _graphShowIds :: Bool,
    _graphShowDecreasingArgs :: A.ShowDecrArgs
  }

parseHtml :: Parser HtmlOptions
parseHtml = do
  _htmlInputFile <-
    argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
      )
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

parseCallGraph :: Parser CallGraphOptions
parseCallGraph = do
  _graphInputFile <-
    argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
      )
  _graphShowIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier"
      )
  _graphShowDecreasingArgs <-
    option decrArgsParser
      ( long "show-decreasing-args"
          <> short 'd'
          <> help "Show the arguments that are detected to decrease"
      )
  pure CallGraphOptions {..}
  where
  decrArgsParser :: ReadM A.ShowDecrArgs
  decrArgsParser = eitherReader $ \s ->
    case map toLower s of
      "argument" -> return A.OnlyArg
      "relation" -> return A.OnlyRel
      "both" -> return A.ArgRel
      _ -> Left "bad argument"



parseParse :: Parser ParseOptions
parseParse = do
  _parseInputFile <-
    argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
      )
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
        commandGraph
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
    commandGraph :: Mod CommandFields Command
    commandGraph = command "calls" minfo
      where
        minfo :: ParserInfo Command
        minfo =
          info
            (CallGraph <$> parseCallGraph)
            (progDesc "Compute the calls graph of a .mjuvix file")

mkScopePrettyOptions :: ScopeOptions -> M.Options
mkScopePrettyOptions ScopeOptions {..} =
  M.defaultOptions
    { M._optShowNameId = _scopeShowIds,
      M._optInlineImports = _scopeInlineImports
    }

mkAbstractPrettyOptions :: CallGraphOptions -> A.Options
mkAbstractPrettyOptions CallGraphOptions {..} =
  A.defaultOptions
    { A._optShowNameId = _graphShowIds,
      A._optShowDecreasingArgs = _graphShowDecreasingArgs
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
go c = case c of
  Scope opts@ScopeOptions {..} -> do
    root <- getCurrentDirectory
    forM_ _scopeInputFiles $ \scopeInputFile -> do
      m <- parseModuleIO scopeInputFile
      s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
      M.printPrettyCode (mkScopePrettyOptions opts) s
  Parse ParseOptions {..} -> do
    m <- parseModuleIO _parseInputFile
    if _parseNoPrettyShow then print m else pPrint m
  Html HtmlOptions {..} -> do
    root <- getCurrentDirectory
    m <- parseModuleIO _htmlInputFile
    s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
    genHtml defaultOptions _htmlRecursive _htmlTheme s
  CallGraph opts@CallGraphOptions {..} -> do
    root <- getCurrentDirectory
    m <- parseModuleIO _graphInputFile
    s <- fromRightIO' printErrorAnsi $ M.scopeCheck1IO root m
    a <- fromRightIO' putStrLn (return $ A.translateModule s)
    let graph = A.buildCallMap a
        opts' = mkAbstractPrettyOptions opts
    A.printPrettyCode opts' graph
    putStrLn ""
    let completeGraph = A.completeCallGraph graph
    A.printPrettyCode opts' completeGraph
    putStrLn ""

main :: IO ()
main = execParser descr >>= go
