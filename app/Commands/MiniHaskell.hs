{-# LANGUAGE ApplicativeDo #-}

module Commands.MiniHaskell where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MiniHaskellOptions = MiniHaskellOptions
  { _miniHaskellInputFile :: FilePath
  }

makeLenses ''MiniHaskellOptions

parseMiniHaskell :: Parser MiniHaskellOptions
parseMiniHaskell = do
  _miniHaskellInputFile <- parserInputFile
  pure MiniHaskellOptions {..}
