{-# LANGUAGE ApplicativeDo #-}
module Commands.MiniHaskell where

import Commands.Extra
import Options.Applicative
import MiniJuvix.Prelude hiding (Doc)

data MiniHaskellOptions = MiniHaskellOptions
  { _mhaskellInputFile :: FilePath
  }

parseMiniHaskell :: Parser MiniHaskellOptions
parseMiniHaskell = do
  _mhaskellInputFile <- parseInputFile
  pure MiniHaskellOptions {..}
