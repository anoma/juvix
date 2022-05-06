{-# LANGUAGE ApplicativeDo #-}

module Commands.MiniC where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MiniCOptions = MiniCOptions
  { _miniCInputFile :: FilePath
  }

makeLenses ''MiniCOptions

parseMiniC :: Parser MiniCOptions
parseMiniC = do
  _miniCInputFile <- parserInputFile
  pure MiniCOptions {..}
