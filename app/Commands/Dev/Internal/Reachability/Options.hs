module Commands.Dev.Internal.Reachability.Options where

import CommonOptions

newtype InternalReachabilityOptions = InternalReachabilityOptions
  { _internalReachabilityInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''InternalReachabilityOptions

parseInternalReachability :: Parser InternalReachabilityOptions
parseInternalReachability = do
  _internalReachabilityInputFile <- parseInputJuvixFile
  pure InternalReachabilityOptions {..}
