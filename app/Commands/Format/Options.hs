module Commands.Format.Options where

import CommonOptions

data FormatOptions = FormatOptions
  { _formatInput :: FilePath,
    _formatCheck :: Bool,
    _formatInPlace :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseInputJuvixFileOrDir :: Parser FilePath
parseInputJuvixFileOrDir =
  strArgument
    ( metavar "JUVIX_FILE_OR_DIR"
        <> help "Path to a .juvix file or to a directory containing Juvix files"
        <> completer juvixCompleter
        <> action "directory"
    )

parseFormat :: Parser FormatOptions
parseFormat = do
  _formatInput <- parseInputJuvixFileOrDir
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
