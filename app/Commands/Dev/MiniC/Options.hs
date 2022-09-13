module Commands.Dev.MiniC.Options where

import CommonOptions

newtype MiniCOptions = MiniCOptions
  { _miniCInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''MiniCOptions

parseMiniC :: Parser MiniCOptions
parseMiniC = do
  _miniCInputFile <- parseInputJuvixFile
  pure MiniCOptions {..}
