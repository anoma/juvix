module Commands.Eval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data EvalOptions = EvalOptions
  { _evalInputFile :: AppPath File,
    _evalSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''EvalOptions

instance CanonicalProjection EvalOptions Core.Options where
  project _ =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = False
      }

instance CanonicalProjection EvalOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. evalInputFile,
        _evalNoIO = False,
        _evalNoDisambiguate = False,
        _evalPrintValues = True
      }

parseEvalOptions :: Parser EvalOptions
parseEvalOptions = do
  _evalInputFile <- parseInputJuvixFile
  _evalSymbolName <-
    optional $
      strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Evaluate a specific function identifier (default: main)"
            <> metavar "NAME"
        )
  pure EvalOptions {..}
