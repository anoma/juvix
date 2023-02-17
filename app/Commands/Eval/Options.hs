module Commands.Eval.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data EvalOptions = EvalOptions
  { _evalShowDeBruijn :: Bool,
    _evalNoIO :: Bool,
    _evalInputFile :: AppPath File,
    _evalSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''EvalOptions

instance CanonicalProjection EvalOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. evalShowDeBruijn
      }

instance CanonicalProjection EvalOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. evalInputFile,
        _evalNoIO = c ^. evalNoIO
      }

parseEvalOptions :: Parser EvalOptions
parseEvalOptions = do
  _evalShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _evalNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
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
