module Core.Normalize.Base where

import Base
import Core.Eval.Base
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pipeline
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource
import Juvix.Data.Field

coreNormalizeAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreNormalizeAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right (_, Nothing) -> assertFailure "Empty program"
    Right (tabIni, Just node) -> do
      step "Transform"
      let tab = setupMainFunction defaultModuleId tabIni node
          transforms = toNormalizeTransformations
      case run $ runReader defaultCoreOptions $ runError @JuvixError $ applyTransformations transforms (moduleFromInfoTable tab) of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right m -> do
          step "Normalize"
          let tab' = computeCombinedInfoTable m
              node' = normalize (maximum allowedFieldSizes) m (lookupIdentifierNode m (fromJust $ tab' ^. infoMain))
              tab'' = setupMainFunction defaultModuleId tab' node'
          coreEvalAssertion' EvalModeJSON tab'' mainFile expectedFile step
