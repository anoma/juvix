module Commands.Dev.Core.FromSource.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreFromSourceOptions = CoreFromSourceOptions
  { _coreFromSourceTransformations :: [TransformationId],
    _coreFromSourceShowDeBruijn :: Bool,
    _coreFromSourceNoIO :: Bool,
    _coreFromSourceEval :: Bool,
    _coreFromSourceInputFile :: AppPath File,
    _coreFromSourceSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''CoreFromSourceOptions

instance CanonicalProjection CoreFromSourceOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreFromSourceShowDeBruijn
      }

instance CanonicalProjection CoreFromSourceOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. coreFromSourceInputFile,
        _evalNoIO = c ^. coreFromSourceNoIO
      }

parseCoreFromSourceOptions :: Parser CoreFromSourceOptions
parseCoreFromSourceOptions = do
  _coreFromSourceTransformations <- optTransformationIds
  _coreFromSourceShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _coreFromSourceEval <-
    switch
      ( long "eval"
          <> help "Evaluate the main function"
      )
  _coreFromSourceNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreFromSourceInputFile <- parseInputJuvixFile
  _coreFromSourceSymbolName <-
    optional $
      strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Print/eval a specific function identifier (default for eval: main)"
            <> metavar "NAME"
        )
  pure CoreFromSourceOptions {..}
