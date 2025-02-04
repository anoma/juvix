module Core.VampIR.Base where

import Base
import Core.Eval.Base
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Transformation.LetHoisting (isLetHoisted)
import Juvix.Compiler.Core.Translation.FromSource

coreVampIRAssertion ::
  [TransformationId] ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreVampIRAssertion transforms mainFile expectedFile step = do
  step "Parse"
  r <- parseFile mainFile
  case r of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right (_, Nothing) -> assertFailure "Empty program"
    Right (tabIni, Just node) -> do
      coreVampIRAssertion' (setupMainFunction defaultModuleId tabIni node) transforms mainFile expectedFile step

coreVampIRAssertion' ::
  InfoTable ->
  [TransformationId] ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreVampIRAssertion' tab transforms mainFile expectedFile step = do
  step "Transform and normalize"
  case run . runReader defaultCoreOptions . runError @JuvixError $
    applyTransformations transforms (moduleFromInfoTable tab) of
    Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
    Right m -> do
      let tab' = computeCombinedInfoTable m
      step "Check let-hoisted"
      walkT checkHoisted tab'
      coreEvalAssertion' EvalModeJSON tab' mainFile expectedFile step
      where
        checkHoisted :: Symbol -> Node -> IO ()
        checkHoisted s n =
          unless (isLetHoisted n) (assertFailure $ "node not hoisted: " <> show s)
