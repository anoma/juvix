module Commands.Dev.Latex.Export.Options where

import CommonOptions
import Prelude qualified

data ExportOptions = ExportOptions
  { _exportInputFile :: AppPath File,
    _exportMode :: ExportMode,
    _exportNoComments :: Bool,
    _exportFromLine :: Maybe Int,
    _exportToLine :: Maybe Int
  }
  deriving stock (Data)

data ExportMode
  = ExportStandalone
  | ExportEnv
  | ExportRaw
  deriving stock (Enum, Bounded, Ord, Eq, Data, Generic)

instance Show ExportMode where
  show = \case
    ExportEnv -> "env"
    ExportStandalone -> "standalone"
    ExportRaw -> "raw"

makeLenses ''ExportOptions

exportModeHelp :: ExportMode -> AnsiDoc
exportModeHelp = \case
  ExportEnv -> "Wrap the code in a Verbatim environment"
  ExportStandalone -> "Output a ready to compile LaTeX file"
  ExportRaw -> "Output only the code"

parseExport :: Parser ExportOptions
parseExport = do
  _exportInputFile <- parseInputFiles (pure FileExtJuvix)
  _exportNoComments <-
    switch
      ( long "no-comments"
          <> help "Do not print comments"
      )
  _exportMode <-
    option
      (enumReader Proxy)
      ( long "mode"
          <> helpDoc ("How to deliver the output:\n" <> enumHelp exportModeHelp)
          <> showDefault
          <> completer (enumCompleter @ExportMode Proxy)
          <> value ExportStandalone
      )
  _exportFromLine <-
    optional $
      option
        readLineNumber
        ( long "from"
            <> metavar "LINE"
            <> help "Output from the given line onwards"
        )
  _exportToLine <-
    optional $
      option
        readLineNumber
        ( long "to"
            <> metavar "LINE"
            <> help "Output until the given line (included)"
        )
  pure ExportOptions {..}
  where
    readLineNumber :: ReadM Int
    readLineNumber = eitherReader readr
      where
        readr :: String -> Either String Int
        readr inputStr = do
          num <- readEither inputStr
          when
            (num <= 0)
            $ Left
              ( "Invalid line number "
                  <> show num
                  <> ". Line number must be at least 1"
              )
          return num
