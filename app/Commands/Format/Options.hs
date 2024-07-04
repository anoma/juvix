module Commands.Format.Options
  ( module Commands.Format.Options,
    module Juvix.Compiler.Concrete.Data.Rename,
  )
where

import CommonOptions
import Juvix.Compiler.Concrete.Data.Rename

data FormatOptions = FormatOptions
  { _formatInput :: Maybe (AppPath FileOrDir),
    _formatRename :: Maybe Rename,
    _formatCheck :: Bool,
    _formatInPlace :: Bool
  }
  deriving stock (Data)

makeLenses ''FormatOptions

parseInputFileOrDir :: Parser (AppPath FileOrDir)
parseInputFileOrDir = do
  _pathPath <-
    argument
      somePreFileOrDirOpt
      ( metavar "JUVIX_FILE_OR_PROJECT"
          <> help ("Path to a " <> show FileExtJuvix <> " file or to a directory containing a Juvix project.")
          <> completer (extCompleter FileExtJuvix)
          <> action "directory"
      )
  pure AppPath {_pathIsInput = True, ..}

parseFormat :: Parser FormatOptions
parseFormat = do
  _formatInput <- optional parseInputFileOrDir
  _formatRename <- optional parseOptRename
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
