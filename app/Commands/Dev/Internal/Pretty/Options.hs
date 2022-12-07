module Commands.Dev.Internal.Pretty.Options where

import CommonOptions

newtype InternalPrettyOptions = InternalPrettyOptions
  { _internalPrettyInputFile :: AppPath File
  }

makeLenses ''InternalPrettyOptions

parseInternalPretty :: Parser InternalPrettyOptions
parseInternalPretty = do
  _internalPrettyInputFile <- parseInputJuvixFile
  pure InternalPrettyOptions {..}
