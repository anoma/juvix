{-# LANGUAGE ApplicativeDo #-}
module Commands.MicroJuvix where

import Commands.Extra
import Options.Applicative
import MiniJuvix.Prelude hiding (Doc)

newtype MicroJuvixOptions = MicroJuvixOptions
  { _mjuvixInputFile :: FilePath
  }

parseMicroJuvix :: Parser MicroJuvixOptions
parseMicroJuvix = do
  _mjuvixInputFile <- parseInputFile
  pure MicroJuvixOptions {..}
