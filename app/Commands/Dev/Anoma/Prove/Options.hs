module Commands.Dev.Anoma.Prove.Options where

import CommonOptions

data ProveOptions = ProveOptions
  { _proveFile :: AppPath File,
    _proveArgs :: Maybe (AppPath File),
    _proveClientInfo :: Maybe (AppPath File),
    _proveOutputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''ProveOptions

parseProveOptions :: Parser ProveOptions
parseProveOptions = do
  _proveFile <- parseInputFile FileExtNockma
  _proveArgs <- optional anomaArgsOpt
  _proveClientInfo <- optional anomaClientConfigOpt
  _proveOutputFile <- optional parseGenericOutputFile
  pure ProveOptions {..}
