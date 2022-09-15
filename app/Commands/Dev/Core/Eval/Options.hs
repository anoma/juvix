module Commands.Dev.Core.Eval.Options where

import CommonOptions
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool,
    _coreEvalInputFile :: Path,
    _coreEvalShowDeBruijn :: Bool
  }
  deriving stock (Data)

makeLenses ''CoreEvalOptions

instance CanonicalProjection CoreEvalOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreEvalShowDeBruijn
      }

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreEvalShowDeBruijn <- optDeBruijn
  _coreEvalInputFile <- parseInputJuvixCoreFile
  pure CoreEvalOptions {..}
