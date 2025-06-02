module Commands.Eval.Options where

import App
import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Backend
import Juvix.Compiler.Core.Pretty.Options qualified as Core
import Juvix.Compiler.Pipeline.EntryPoint

data EvalOptions = EvalOptions
  { _evalInputFile :: Maybe (AppPath File),
    _evalSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''EvalOptions

instance CanonicalProjection EvalOptions Core.Options where
  project _ =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = False
      }

instance EntryPointOptions EvalOptions where
  applyOptions _ =
    set entryPointPipeline (Just PipelineEval)
      . set entryPointTarget (Just TargetCore)

evalOptionsToEvalOptions :: (Members '[App] r) => EvalOptions -> Sem r Eval.EvalOptions
evalOptionsToEvalOptions c = do
  inputFile <- getMainAppFile (c ^. evalInputFile)
  return
    Eval.EvalOptions
      { _evalInputFile = inputFile,
        _evalNoIO = False,
        _evalNoDisambiguate = False,
        _evalPrintValues = True
      }

parseEvalOptions :: Parser EvalOptions
parseEvalOptions = do
  _evalInputFile <- optional (parseInputFiles (FileExtJuvix :| [FileExtJuvixMarkdown]))
  _evalSymbolName <-
    optional
      $ strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Evaluate a specific function identifier (default: main)"
            <> metavar "NAME"
        )
  pure EvalOptions {..}
