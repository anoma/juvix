module Commands.Dev.Core.Eval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool,
    _coreEvalInputFile :: AppPath File,
    _coreEvalShowDeBruijn :: Bool
  }

makeLenses ''CoreEvalOptions

instance CanonicalProjection CoreEvalOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreEvalShowDeBruijn
      }

instance CanonicalProjection CoreEvalOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. coreEvalInputFile,
        _evalNoIO = c ^. coreEvalNoIO
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
