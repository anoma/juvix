{-# LANGUAGE ApplicativeDo #-}

module Commands.MicroJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MicroJuvixOptions = MicroJuvixOptions
  { _mjuvixInputFile :: FilePath
  }

parseMicroJuvix :: Parser MicroJuvixOptions
parseMicroJuvix = do
  _mjuvixInputFile <- parseInputFile
  pure MicroJuvixOptions {..}
