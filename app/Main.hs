{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Monad.Extra
import qualified MiniJuvix.Syntax.Concrete.Language as M
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as M
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (Options (..))
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M
import MiniJuvix.Utils.Prelude
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.IO.Error
import Text.Show.Pretty hiding (Html)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (defaultOptions)

data Command
  = Scope ScopeOptions
  | Parse ParseOptions
  | Html HtmlOptions

data ScopeOptions = ScopeOptions
  { _scopeRootDir :: FilePath,
    _scopeInputFile :: FilePath,
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
  _scopeInputFile <-
    argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
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
        commandHtml
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

mkPrettyOptions :: ScopeOptions -> M.Options
mkPrettyOptions ScopeOptions {..} =
  M.defaultOptions
    { _optShowNameId = _scopeShowIds,
      _optInlineImports = _scopeInlineImports
    }

parseModuleIO :: FilePath -> IO (M.Module 'M.Parsed 'M.ModuleTop)
parseModuleIO = fromRightIO id . M.runModuleParserIO

fromRightIO :: (e -> Text) -> IO (Either e r) -> IO r
fromRightIO pp = eitherM (ioError . userError . unpack . pp) return

go :: Command -> IO ()
go c = case c of
  Scope opts@ScopeOptions {..} -> do
    root <- getCurrentDirectory
    m <- parseModuleIO _scopeInputFile
    s <- fromRightIO show $ M.scopeCheck1 root m
    M.printTopModule (mkPrettyOptions opts) s
  Parse ParseOptions {..} -> do
    m <- parseModuleIO _parseInputFile
    if _parseNoPrettyShow then print m else pPrint m
  Html HtmlOptions {..} -> do
    root <- getCurrentDirectory
    m <- parseModuleIO _htmlInputFile
    s <- fromRightIO show $ M.scopeCheck1 root m
    genHtml defaultOptions _htmlRecursive _htmlTheme s

main :: IO ()
main = execParser descr >>= go
