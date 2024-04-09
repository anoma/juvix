module BackendGeb.FromCore.Base where

import Base
import GHC.Base (seq)
import Juvix.Compiler.Backend (Target (TargetGeb))
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Prelude.Pretty

coreToGebTranslationAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreToGebTranslationAssertion root mainFile expectedFile step = do
  step "Parse Juvix Core file"
  input_ <- readFile mainFile
  entryPoint <- set entryPointTarget (Just TargetGeb) <$> testDefaultEntryPointIO root mainFile
  case Core.runParserMain mainFile defaultModuleId mempty input_ of
    Left err -> assertFailure . show . pretty $ err
    Right coreInfoTable -> coreToGebTranslationAssertion' coreInfoTable entryPoint expectedFile step

coreToGebTranslationAssertion' ::
  Core.InfoTable ->
  EntryPoint ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
coreToGebTranslationAssertion' coreInfoTable entryPoint expectedFile step = do
  step "Prepare the Juvix Core node for translation to Geb"
  case run . runReader entryPoint . runError @Geb.JuvixError $ Core.toGeb (Core.moduleFromInfoTable coreInfoTable) of
    Left err ->
      assertFailure . show . pretty $
        fromJuvixError @GenericError err
    Right readyCoreModule ->
      let readyCoreInfoTable = Core.computeCombinedInfoTable readyCoreModule
       in length (fromText (Core.ppTrace readyCoreInfoTable) :: String) `seq` do
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
                let (gebCoreEvalResult, _) = Geb.fromCore $ Core.setupMainFunction defaultModuleId readyCoreInfoTable resultCoreEval
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
                              expectedInput <- readFile expectedFile
                              step "Compare expected and actual program output"
                              let compareEvalOutput morph =
                                    if
                                        | Geb.quote resEvalTranslatedMorph /= morph ->
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
