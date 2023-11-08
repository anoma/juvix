module Commands.Dev.DisplayRoot.Options where

import CommonOptions

data RootOptions = RootOptions
  { _rootPrintPackage :: Bool,
    _rootMainFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''RootOptions

parseRoot :: Parser RootOptions
parseRoot = do
  _rootPrintPackage <-
    switch
      ( long "print-package"
          <> help "print the Package.juvix file as parsed"
      )

  _rootMainFile <- optional (parseInputFile FileExtJuvix)
  pure RootOptions {..}
