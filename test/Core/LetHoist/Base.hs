module Core.LetHoist.Base where

import Base
import Core.Eval.Base
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Transformation.LetHoisting (isLetHoisted)
import Juvix.Compiler.Core.Translation.FromSource

coreLetHoistAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreLetHoistAssertion mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (show (pretty err))
    Right (_, Nothing) -> assertFailure "Empty program"
    Right (tabIni, Just node) -> do
      step "Transform"
      let tab = setupMainFunction tabIni node
          transforms = toLetHoistTransformations
      case run . runReader defaultCoreOptions . runError @JuvixError $
        applyTransformations transforms tab of
        Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
        Right tab' -> do
          step "Is let hoisted"
          walkT checkHoisted tab'
          let node' = normalize tab' (lookupIdentifierNode tab' (fromJust $ tab' ^. infoMain))
              tab'' = setupMainFunction tab' node'
          coreEvalAssertion' EvalModeJSON tab'' mainFile expectedFile step
          where
            checkHoisted :: Symbol -> Node -> IO ()
            checkHoisted s n =
              unless (isLetHoisted n) (assertFailure $ "node not hoisted: " <> show s)
