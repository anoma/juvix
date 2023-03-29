module Commands.Dev.Core.Eval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreEvalOptions = CoreEvalOptions
  { _coreEvalNoIO :: Bool,
    _coreEvalInputFile :: AppPath File,
    _coreEvalShowDeBruijn :: Bool,
    _coreEvalShowIdentIds :: Bool,
    _coreEvalShowArgsNum :: Bool,
    _coreEvalNoDisambiguate :: Bool
  }
  deriving stock (Data)

makeLenses ''CoreEvalOptions

instance CanonicalProjection CoreEvalOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreEvalShowDeBruijn,
        Core._optShowIdentIds = c ^. coreEvalShowIdentIds,
        Core._optShowArgsNum = c ^. coreEvalShowArgsNum
      }

instance CanonicalProjection CoreEvalOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. coreEvalInputFile,
        _evalNoIO = c ^. coreEvalNoIO,
        _evalNoDisambiguate = c ^. coreEvalNoDisambiguate
      }

parseCoreEvalOptions :: Parser CoreEvalOptions
parseCoreEvalOptions = do
  _coreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreEvalShowDeBruijn <- optDeBruijn
  _coreEvalShowIdentIds <- optIdentIds
  _coreEvalShowArgsNum <- optArgsNum
  _coreEvalNoDisambiguate <- optNoDisambiguate
  _coreEvalInputFile <- parseInputJuvixCoreFile
  pure CoreEvalOptions {..}
