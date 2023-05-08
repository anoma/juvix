module Core.Normalize.Base where

import Base
import Core.Eval.Base
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pipeline
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource

coreNormalizeAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreNormalizeAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (_, Nothing) -> assertFailure "Empty program"
    Right (tabIni, Just node) -> do
      step "Transform"
      let tab = setupMainFunction tabIni node
          transforms = toNormalizeTransformations
      case run $ runReader defaultCoreOptions $ runError @JuvixError $ applyTransformations transforms tab of
        Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
        Right tab' -> do
          step "Normalize"
          let node' = normalize tab' (lookupIdentifierNode tab' (fromJust $ tab' ^. infoMain))
              tab'' = setupMainFunction tab' node'
          coreEvalAssertion' EvalModeJSON tab'' mainFile expectedFile step
