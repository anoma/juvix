module Commands.Dev.DevCompile.Core.Options
  ( module Commands.Dev.DevCompile.Core.Options,
    module Commands.Compile.CommonOptions,
  )
where

import App
import Commands.Compile.CommonOptions
import CommonOptions
import Evaluator qualified as Eval
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreOptions = CoreOptions
  { _coreCompileCommonOptions :: CompileCommonOptionsMain,
    _coreTransformations :: [TransformationId],
    _coreShowDeBruijn :: Bool,
    _coreShowIdentIds :: Bool,
    _coreShowArgsNum :: Bool,
    _coreNoDisambiguate :: Bool,
    _coreFilter :: Bool,
    _coreNoIO :: Bool,
    _coreEval :: Bool,
    _coreNormalize :: Bool,
    _coreSymbolName :: Maybe Text
  }
  deriving stock (Data)

makeLenses ''CoreOptions

instance CanonicalProjection CoreOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreShowDeBruijn,
        Core._optShowIdentIds = c ^. coreShowIdentIds,
        Core._optShowArgsNum = c ^. coreShowArgsNum
      }

coreOptionsToEvalOptions :: (Members '[App] r) => CoreOptions -> Sem r Eval.EvalOptions
coreOptionsToEvalOptions c = do
  inputFile <- getMainAppFile (c ^. coreCompileCommonOptions . compileInputFile)
  return
    Eval.EvalOptions
      { _evalInputFile = inputFile,
        _evalNoIO = c ^. coreNoIO,
        _evalNoDisambiguate = c ^. coreNoDisambiguate,
        _evalPrintValues = False
      }

parseCoreOptions :: Parser CoreOptions
parseCoreOptions = do
  _coreTransformations <- optCoreTransformationIds
  _coreShowDeBruijn <- optDeBruijn
  _coreShowIdentIds <- optIdentIds
  _coreShowArgsNum <- optArgsNum
  _coreNoDisambiguate <- optNoDisambiguate
  _coreFilter <-
    switch
      ( long "filter"
          <> help "Filter out the functions not from the input module"
      )
  _coreEval <-
    switch
      ( long "eval"
          <> help "Evaluate the main function"
      )
  _coreNormalize <-
    switch
      ( long "normalize"
          <> help "Normalize the main function"
      )
  _coreNoIO <-
    switch
      ( long "no-io"
          <> help "Don't interpret the IO effects"
      )
  _coreSymbolName <-
    optional $
      strOption
        ( long "symbol-name"
            <> short 's'
            <> help "Print/eval a specific function identifier (default for eval: main)"
            <> metavar "NAME"
        )
  _coreCompileCommonOptions <- parseCompileCommonOptionsMain
  pure CoreOptions {..}
