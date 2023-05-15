module Commands.Dev.Core.Normalize.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreNormalizeOptions = CoreNormalizeOptions
  { _coreNormalizeInputFile :: AppPath File,
    _coreNormalizeShowDeBruijn :: Bool,
    _coreNormalizeShowIdentIds :: Bool,
    _coreNormalizeShowArgsNum :: Bool,
    _coreNormalizeNoDisambiguate :: Bool
  }
  deriving stock (Data)

makeLenses ''CoreNormalizeOptions

instance CanonicalProjection CoreNormalizeOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreNormalizeShowDeBruijn,
        Core._optShowIdentIds = c ^. coreNormalizeShowIdentIds,
        Core._optShowArgsNum = c ^. coreNormalizeShowArgsNum
      }

instance CanonicalProjection CoreNormalizeOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. coreNormalizeInputFile,
        _evalNoIO = True,
        _evalNoDisambiguate = c ^. coreNormalizeNoDisambiguate
      }

parseCoreNormalizeOptions :: Parser CoreNormalizeOptions
parseCoreNormalizeOptions = do
  _coreNormalizeShowDeBruijn <- optDeBruijn
  _coreNormalizeShowIdentIds <- optIdentIds
  _coreNormalizeShowArgsNum <- optArgsNum
  _coreNormalizeNoDisambiguate <- optNoDisambiguate
  _coreNormalizeInputFile <- parseInputJuvixCoreFile
  pure CoreNormalizeOptions {..}
