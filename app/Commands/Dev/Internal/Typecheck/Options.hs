module Commands.Dev.Internal.Typecheck.Options where

import CommonOptions

data InternalTypeOptions = InternalTypeOptions
  { _internalTypePrint :: Bool,
    _internalTypeInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''InternalTypeOptions

parseInternalType :: Parser InternalTypeOptions
parseInternalType = do
  _internalTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  _internalTypeInputFile <- parseInputFile FileExtJuvix
  pure InternalTypeOptions {..}
