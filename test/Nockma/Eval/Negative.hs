module Nockma.Eval.Negative where

import Base hiding (Path, testName)
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree
import Nockma.Eval.Positive qualified as Pos

negAnomaTest ::
  Text ->
  Sem '[Reader CompilerCtx] (Term Natural) ->
  [Term Natural] ->
  (NockEvalError Natural -> Bool) ->
  Pos.Test
negAnomaTest name mainFun args checkErr =
  let _testName :: Text
        | _evalInterceptAnomaLibCalls = name <> " - intercept stdlib"
        | otherwise = name
      _evalInterceptAnomaLibCalls = True
      _testProgramSubject = makeMainFunction True (fromIntegral (length args)) (Pos.compileTerm mainFun)
      _testProgramFormula = anomaCall args
      _testProgramStorage :: Storage Natural = emptyStorage
      _testEvalOptions = EvalOptions {..}
      _testAssertEvalError = Just $ \e ->
        if
            | checkErr e -> return ()
            | otherwise -> error "Incorrect error"
      _testCheck :: Pos.Check () = return ()
   in Pos.Test {..}

allTests :: TestTree
allTests =
  testGroup
    "Nockma eval negative"
    $ map
      Pos.mkNockmaTest
      [ do
          negAnomaTest
            "randomBits not divisible by 8"
            ( do
                gen <- callStdlib StdlibRandomInitGen [nockNatLiteral 99]
                callStdlib StdlibRandomNextBits [gen, nockNatLiteral 1]
            )
            []
            $ \case
              ErrCantGenerateRandomBits {} -> True
              _ -> False
      ]
