module BackendGeb.FromCore.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Core qualified as Core
import Juvix.Prelude.Pretty

{-
  Test summary:
  1. Parse the Juvix Core file.
  2. Prepare the Juvix Core node for translation to Geb.
  3. Translate the Juvix Core node to Geb so that it can be executed in the Geb
     environment.
  4. Perform typechecking on the translated Geb node to ensure that the types
     from the core node make sense in the Geb context and avoid any runtime
     errors.
  5. Evaluate the Juvix Core node to see if it produces the expected output.
  6. Translate the result of the evaluated Juvix Core node to Geb for comparison
     with the expected output.
  7. Compare the result of the evaluation of the Geb terms produced in step 3
     with the result of the evaluation of the Geb terms produced in step 6 to
     ensure consistency.
  8. If step 7 succeeds, then compare the output of the evaluation of the core
     node with the expected output to ensure that the program is functioning as
     intended.
-}

coreToGebtranslationAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreToGebtranslationAssertion mainFile expectedFile step = do
  step "Parse Juvix Core file"
  input <- readFile . toFilePath $ mainFile
  case Core.runParserMain mainFile Core.emptyInfoTable input of
    Left err -> assertFailure . show . pretty $ err
    Right coreInfoTable -> do
      step "Prepare the Juvix Core node for translation to Geb"
      case run . runError @Geb.JuvixError $ Core.toGeb coreInfoTable of
        Left err ->
          assertFailure . show . pretty $
            fromJuvixError @GenericError err
        Right readyCoreInfoTable -> do
          step "Translate the Juvix Core node to Geb"
          let (translatedMorphism, translatedObj) = Geb.fromCore readyCoreInfoTable
          step "Typecheck the translated Geb node"
          let typeMorph =
                Geb.TypedMorphism
                  { _typedMorphism = translatedMorphism,
                    _typedMorphismObject = translatedObj
                  }
          case run . runError @Geb.CheckingError $ Geb.check' typeMorph of
            Left err ->
              assertFailure . show . pretty $
                fromJuvixError @GenericError (JuvixError err)
            Right _ -> do
              step "Try evaluating the JuvixCore node"
              let resultCoreEval :: Core.Node = Core.evalInfoTable stderr readyCoreInfoTable
              step "Translate the result of the evaluated JuvixCore node to Geb"
              let (gebCoreEvalResult, _) = Geb.fromCore $ Core.setupMainFunction readyCoreInfoTable resultCoreEval
              case ( Geb.eval' Geb.defaultEvalEnv translatedMorphism,
                     Geb.eval' Geb.defaultEvalEnv gebCoreEvalResult
                   ) of
                (Left err, _) -> do
                  step "The evaluation of the translated Geb node failed"
                  assertFailure . show . pretty $
                    fromJuvixError @GenericError (JuvixError err)
                (_, Left err) -> do
                  step "The evaluation of gebCoreEvalResult failed"
                  assertFailure . show . pretty $ fromJuvixError @GenericError (JuvixError err)
                ( Right resEvalTranslatedMorph,
                  Right resEvalGebCoreEvalResult
                  ) -> do
                    step "Compare the geb value of the Core eval output and the Geb eval output"
                    if
                        | resEvalTranslatedMorph /= resEvalGebCoreEvalResult ->
                            assertFailure "The evaluation for the Core node and the Geb node are not equal"
                        | otherwise -> do
                            let fpath = toFilePath expectedFile
                            expectedInput <- TIO.readFile fpath
                            step "Compare expected and actual program output"
                            let compareEvalOutput morph =
                                  case Geb.eval' Geb.defaultEvalEnv morph of
                                    Left err ->
                                      assertFailure . show . pretty $
                                        fromJuvixError @GenericError (JuvixError err)
                                    Right resEvalExpected -> do
                                      if
                                          | resEvalTranslatedMorph /= resEvalExpected ->
                                              assertFailure $
                                                "The result of evaluating the translated Geb"
                                                  <> "node is not equal to the expected output"
                                          | otherwise -> assertBool "" True
                            case Geb.runParser expectedFile expectedInput of
                              Left parseErr -> assertFailure . show . pretty $ parseErr
                              Right (Geb.ExpressionMorphism m) -> compareEvalOutput m
                              Right (Geb.ExpressionTypedMorphism m) -> compareEvalOutput (m ^. Geb.typedMorphism)
                              Right (Geb.ExpressionObject _) ->
                                assertFailure "Expected a morphism, but got an object for the expected output"
