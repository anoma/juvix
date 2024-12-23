module Commands.Dev.Anoma.Prove.Options
  ( module Commands.Dev.Anoma.Prove.Options,
    module Commands.Dev.Anoma.Prove.Options.ProveArg,
  )
where

import Commands.Dev.Anoma.Prove.Options.ProveArg
import CommonOptions

data ProveOptions = ProveOptions
  { _proveFile :: AppPath File,
    _proveArgs :: [ProveArg],
    _proveOutputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''ProveOptions

parseProveOptions :: Parser ProveOptions
parseProveOptions = do
  _proveFile <- parseInputFile FileExtNockma
  _proveArgs <- many parseProveArg
  _proveOutputFile <- optional parseGenericOutputFile
  pure ProveOptions {..}
