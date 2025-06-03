module Commands.Dev.DisplayBuildDir.Options where

import CommonOptions

data BuildDirOptions = BuildDirOptions
  { _buildDirRelative :: Bool
  }
  deriving stock (Data)

makeLenses ''BuildDirOptions

parseBuildDir :: Parser BuildDirOptions
parseBuildDir = do
  _buildDirRelative <-
    switch
      ( long "relative-path"
          <> help "print a relative path wrt the project root"
      )

  pure BuildDirOptions {..}
