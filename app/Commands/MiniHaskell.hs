{-# LANGUAGE ApplicativeDo #-}

module Commands.MiniHaskell where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MiniHaskellOptions = MiniHaskellOptions
  { _mhaskellInputFile :: FilePath
  }

parseMiniHaskell :: Parser MiniHaskellOptions
parseMiniHaskell = do
  _mhaskellInputFile <- parseInputFile
  pure MiniHaskellOptions {..}
