module Commands.Dev.Internal.Pretty.Options where

import CommonOptions

newtype InternalPrettyOptions = InternalPrettyOptions
  { _internalPrettyInputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

makeLenses ''InternalPrettyOptions

parseInternalPretty :: Parser InternalPrettyOptions
parseInternalPretty = do
  _internalPrettyInputFile <- optional (parseInputFile FileExtJuvix)
  pure InternalPrettyOptions {..}
