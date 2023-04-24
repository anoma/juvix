module Commands.Format.Options where

import CommonOptions

data FormatOptions = FormatOptions
  { _formatInput :: Maybe (Prepath FileOrDir),
    _formatCheck :: Bool,
    _formatInPlace :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseInputJuvixFileOrDir :: Parser (Prepath FileOrDir)
parseInputJuvixFileOrDir =
  strArgument
    ( metavar "JUVIX_FILE_OR_PROJECT"
        <> help "Path to a .juvix file or to a directory containing a Juvix project."
        <> completer juvixCompleter
        <> action "directory"
    )

parseFormat :: Parser FormatOptions
parseFormat = do
  _formatInput <- optional parseInputJuvixFileOrDir
  _formatCheck <-
    switch
      ( long "check"
          <> help "Do not print reformatted sources or unformatted file paths to standard output."
      )
  _formatInPlace <-
    switch
      ( long "in-place"
          <> help "Do not print reformatted sources to standard output. Overwrite the target's contents with the formatted version if the formatted version differs from the original content."
      )
  pure FormatOptions {..}
