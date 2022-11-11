module Commands.Dev.Core.Read.Options where

import Commands.Dev.Core.Eval.Options qualified as Eval
import CommonOptions
import Evaluator qualified
import Juvix.Compiler.Core.Data.TransformationId.Parser
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreReadOptions = CoreReadOptions
  { _coreReadTransformations :: [TransformationId],
    _coreReadShowDeBruijn :: Bool,
    _coreReadEval :: Bool,
    _coreReadNoPrint :: Bool,
    _coreReadInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''CoreReadOptions

instance CanonicalProjection CoreReadOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReadShowDeBruijn
      }

instance CanonicalProjection CoreReadOptions Eval.CoreEvalOptions where
  project c =
    Eval.CoreEvalOptions
      { _coreEvalNoIO = False,
        _coreEvalInputFile = c ^. coreReadInputFile,
        _coreEvalShowDeBruijn = c ^. coreReadShowDeBruijn
      }

instance CanonicalProjection CoreReadOptions Evaluator.EvalOptions where
  project x =
    Evaluator.EvalOptions
      { _evalNoIO = False,
        _evalInputFile = x ^. coreReadInputFile
      }

parseCoreReadOptions :: Parser CoreReadOptions
parseCoreReadOptions = do
  _coreReadShowDeBruijn <- optDeBruijn
  _coreReadNoPrint <-
    switch
      ( long "no-print"
          <> help "do not print the transformed code"
      )
  _coreReadEval <-
    switch
      ( long "eval"
          <> help "evaluate after the transformation"
      )
  _coreReadTransformations <-
    option
      (eitherReader parseTransf)
      ( long "transforms"
          <> short 't'
          <> value mempty
          <> metavar "[Transform]"
          <> help "comma sep list of transformations. Available: lifting, top-eta-expand, identity"
      )
  _coreReadInputFile <- parseInputJuvixCoreFile
  pure CoreReadOptions {..}
  where
    parseTransf :: String -> Either String [TransformationId]
    parseTransf = mapLeft unpack . parseTransformations . pack
