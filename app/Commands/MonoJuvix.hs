{-# LANGUAGE ApplicativeDo #-}

module Commands.MonoJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MonoJuvixOptions = MonoJuvixOptions
  { _monoJuvixInputFile :: FilePath
  }

makeLenses ''MonoJuvixOptions

parseMonoJuvix :: Parser MonoJuvixOptions
parseMonoJuvix = do
  _monoJuvixInputFile <- parserInputFile
  pure MonoJuvixOptions {..}
