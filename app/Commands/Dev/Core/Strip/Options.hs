module Commands.Dev.Core.Strip.Options where

import CommonOptions
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreStripOptions = CoreStripOptions
  { _coreStripShowDeBruijn :: Bool,
    _coreStripNoPrint :: Bool,
    _coreStripInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''CoreStripOptions

instance CanonicalProjection CoreStripOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreStripShowDeBruijn
      }

parseCoreStripOptions :: Parser CoreStripOptions
parseCoreStripOptions = do
  _coreStripShowDeBruijn <- optDeBruijn
  _coreStripNoPrint <-
    switch
      ( long "no-print"
          <> help "do not print the stripped code"
      )
  _coreStripInputFile <- parseInputJuvixCoreFile
  pure CoreStripOptions {..}
