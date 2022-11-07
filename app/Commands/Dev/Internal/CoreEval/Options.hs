module Commands.Dev.Internal.CoreEval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data InternalCoreEvalOptions = InternalCoreEvalOptions
  { _internalCoreEvalShowDeBruijn :: Bool,
    _internalCoreEvalNoIO :: Bool,
    _internalCoreEvalInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''InternalCoreEvalOptions

instance CanonicalProjection InternalCoreEvalOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. internalCoreEvalShowDeBruijn
      }

instance CanonicalProjection InternalCoreEvalOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. internalCoreEvalInputFile,
        _evalNoIO = c ^. internalCoreEvalNoIO
      }

parseInternalCoreEval :: Parser InternalCoreEvalOptions
parseInternalCoreEval = do
  _internalCoreEvalShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _internalCoreEvalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _internalCoreEvalInputFile <- parseInputJuvixFile
  pure InternalCoreEvalOptions {..}
