{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import MiniJuvix.Utils.Prelude
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as M
import Options.Applicative
import Options.Applicative.Help.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (Options(_optShowNameId))

data Command =
  Scope ScopeOptions
  | Parse ParseOptions

data ScopeOptions = ScopeOptions {
  _scopeRootDir :: FilePath
  , _scopeInputFile :: FilePath
  , _scopeShowIds :: Bool
  }

data ParseOptions = ParseOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeRootDir <- strOption
    (long "rootDir"
     <> short 'd'
     <> metavar "DIR"
     <> value "."
     <> showDefault
     <> help "Root directory")
  _scopeInputFile <- argument str
     (metavar "MINIJUVIX_FILE"
     <> help "Path to a .mjuvix file"
     )
  _scopeShowIds <- switch
     ( long "show-name-ids"
     <> help "Show the unique number of each identifier"
     )

  pure ScopeOptions {..}

parseParse :: Parser ParseOptions
parseParse = pure ParseOptions

descr :: ParserInfo Command
descr = info (parseCommand <**> helper)
       (fullDesc
        <> progDesc "The MiniJuvix compiler."
        <> headerDoc (Just $ dullblue $ bold $ underline "MiniJuvix help")
        <> footerDoc (Just foot)
       )
  where
  foot :: Doc
  foot = bold "maintainers: " <> "jan@heliax.dev; jonathan@heliax.dev"

parseCommand :: Parser Command
parseCommand = subparser (
   command "parse" (info (Parse <$> parseParse) (progDesc "Parse some .mjuvix files"))
   <> command "scope" (info (Scope <$> parseScope) (progDesc "Parse and scope some .mjuvix files"))
                    )

mkPrettyOptions :: ScopeOptions -> M.Options
mkPrettyOptions ScopeOptions {..} = M.defaultOptions {
  _optShowNameId = _scopeShowIds
  }

go :: Command -> IO ()
go c = case c of
  Scope opts@ScopeOptions {..} -> do
    res <- M.runModuleParserIO _scopeInputFile
    case res of
      Left err -> print err
      Right m -> do
        print m
        putStrLn "\n\n"
        s <- M.scopeCheck _scopeInputFile [m]
        case s of
          Left err -> print err
          Right [r] -> M.printTopModule (mkPrettyOptions opts) r
          Right _ -> error "impossible"
  Parse _ -> putStrLn "not implemented"

main :: IO ()
main = execParser descr >>= go
