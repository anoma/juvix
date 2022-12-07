module Commands.Dev.Internal.Arity.Options where

import CommonOptions

newtype InternalArityOptions = InternalArityOptions
  { _internalArityInputFile :: AppPath File
  }

makeLenses ''InternalArityOptions

parseInternalArity :: Parser InternalArityOptions
parseInternalArity = do
  _internalArityInputFile <- parseInputJuvixFile
  pure InternalArityOptions {..}
