module Commands.Dev.Internal.Typecheck.Options where

import CommonOptions

data InternalTypeOptions = InternalTypeOptions
  { _internalTypePrint :: Bool,
    _internalTypeInputFile :: Maybe (AppPath File)
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
  _internalTypeInputFile <- optional (parseInputFile FileExtJuvix)
  pure InternalTypeOptions {..}
