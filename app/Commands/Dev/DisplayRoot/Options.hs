module Commands.Dev.DisplayRoot.Options where

import CommonOptions

newtype RootOptions = RootOptions
  { _rootPrintPackage :: Bool
  }
  deriving stock (Data)

makeLenses ''RootOptions

parseRoot :: Parser RootOptions
parseRoot = do
  _rootPrintPackage <-
    switch
      ( long "print-package"
          <> help "print the juvix.yaml file as parsed"
      )
  pure RootOptions {..}
