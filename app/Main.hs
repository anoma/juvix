{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import MiniJuvix.Utils.Prelude
import qualified MiniJuvix.Syntax.Concrete.Parser as M
import qualified MiniJuvix.Syntax.Scoped.Scoper as M
import Options.Applicative
import Options.Applicative.Help.Pretty

data Command =
  Scope ScopeOptions
  | Parse ParseOptions

data ScopeOptions = ScopeOptions {
  _rootDir :: FilePath
  , _inputFile :: FilePath
  }

data ParseOptions = ParseOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _rootDir <- strOption
    (long "rootDir"
     <> short 'd'
     <> metavar "DIR"
     <> value "."
     <> showDefault
     <> help "Root directory")
  _inputFile <- argument str
     (metavar "MINIJUVIX_FILE"
     <> help "Path to a .mjuvix file"
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

go :: Command -> IO ()
go c = case c of
  Scope ScopeOptions {..} -> do
    res <- M.runModuleParserIO _inputFile
    case res of
      Left err -> print err
      Right m -> do
        print m
        putStrLn "\n\n"
        s <- M.scopeCheck _inputFile [m]
        case s of
          Left err -> print err
          Right r -> print r
  Parse _ -> putStrLn "not implemented"

main :: IO ()
main = execParser descr >>= go
