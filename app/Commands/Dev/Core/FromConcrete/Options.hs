module Commands.Dev.Core.FromConcrete.Options where

import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreFromConcreteOptions = CoreFromConcreteOptions
  { _coreFromConcreteTransformations :: [TransformationId],
    _coreFromConcreteShowDeBruijn :: Bool,
    _coreFromConcreteFilter :: Bool,
    _coreFromConcreteNoIO :: Bool,
    _coreFromConcreteEval :: Bool,
    _coreFromConcreteInputFile :: AppPath File,
    _coreFromConcreteSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''CoreFromConcreteOptions

instance CanonicalProjection CoreFromConcreteOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreFromConcreteShowDeBruijn
      }

instance CanonicalProjection CoreFromConcreteOptions Eval.EvalOptions where
  project c =
    Eval.EvalOptions
      { _evalInputFile = c ^. coreFromConcreteInputFile,
        _evalNoIO = c ^. coreFromConcreteNoIO
      }

parseCoreFromConcreteOptions :: Parser CoreFromConcreteOptions
parseCoreFromConcreteOptions = do
  _coreFromConcreteTransformations <- optTransformationIds
  _coreFromConcreteShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _coreFromConcreteFilter <-
    switch
      ( long "filter"
          <> help "Filter out the functions not from the input module"
      )
  _coreFromConcreteEval <-
    switch
      ( long "eval"
          <> help "Evaluate the main function"
      )
  _coreFromConcreteNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreFromConcreteInputFile <- parseInputJuvixFile
  _coreFromConcreteSymbolName <-
    optional $
      strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Print/eval a specific function identifier (default for eval: main)"
            <> metavar "NAME"
        )
  pure CoreFromConcreteOptions {..}
