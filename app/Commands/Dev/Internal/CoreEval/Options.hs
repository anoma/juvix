module Commands.Dev.Internal.CoreEval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data InternalCoreEvalOptions = InternalCoreEvalOptions
  { _internalCoreEvalTransformations :: [TransformationId],
    _internalCoreEvalShowDeBruijn :: Bool,
    _internalCoreEvalNoIO :: Bool,
    _internalCoreEvalInputFile :: AppPath File,
    _internalCoreEvalSymbolName :: Maybe Text
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
  _internalCoreEvalTransformations <- optTransformationIds
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
  _internalCoreEvalSymbolName <-
    optional $
      strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Evaluate a function identifier (default: main)"
            <> metavar "NAME"
        )
  pure InternalCoreEvalOptions {..}
