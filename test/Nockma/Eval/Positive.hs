module Nockma.Eval.Positive where

import Base hiding (Path)
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource.QQ

type Check = Sem '[Reader [Term Natural], Reader (Term Natural), Embed IO]

data Test = Test
  { _testName :: Text,
    _testProgramSubject :: Term Natural,
    _testProgramFormula :: Term Natural,
    _testCheck :: Check ()
  }

makeLenses ''Test

allTests :: TestTree
allTests = testGroup "Nockma eval unit positive" (map mk tests)
  where
    mk :: Test -> TestTree
    mk Test {..} = testCase (unpack _testName) $ do
      let (traces, evalResult) =
            run
              . runReader defaultEvalOptions
              . runOutputList @(Term Natural)
              . runError @(ErrNockNatural Natural)
              . runError @(NockEvalError Natural)
              $ eval _testProgramSubject _testProgramFormula
      case evalResult of
        Left natErr -> assertFailure ("Evaluation error: " <> show natErr)
        Right r -> case r of
          Left evalErr -> assertFailure ("Evaluation error: " <> unpack (ppTrace evalErr))
          Right res -> runM (runReader res (runReader traces _testCheck))

eqNock :: Term Natural -> Check ()
eqNock expected = do
  actual <- ask
  unless (expected == actual) (err actual)
  where
    err :: Term Natural -> Check ()
    err actual = do
      let msg =
            "Expected:\n"
              <> ppTrace expected
              <> "\nBut got:\n"
              <> ppTrace actual
      assertFailure (unpack msg)

eqTraces :: [Term Natural] -> Check ()
eqTraces expected = do
  ts <- ask
  unless (ts == expected) (err ts)
  where
    err :: [Term Natural] -> Check ()
    err ts = do
      let msg =
            "Expected traces:\n"
              <> ppTrace expected
              <> "\nBut got:\n"
              <> ppTrace ts
      assertFailure (unpack msg)

tests :: [Test]
tests =
  [ Test "address" [nock| [0 1] |] [nock| [[@ R] [@ L]] |] (eqNock [nock| [1 0] |]),
    Test "address nested" [nock| [0 1 2 3 4 5] |] [nock| [@ RRRRR] |] (eqNock [nock| 5 |]),
    Test "quote" [nock| [0 1] |] [nock| [quote [1 0]] |] (eqNock [nock| [1 0] |]),
    Test "apply" [nock| [0 1] |] [nock| [apply [@ S] [quote [@ R]]] |] (eqNock [nock| 1 |]),
    Test "isCell atom" [nock| [0 1] |] [nock| [isCell [@ L]] |] (eqNock [nock| false |]),
    Test "isCell cell" [nock| [0 1] |] [nock| [isCell [@ S]] |] (eqNock [nock| true |]),
    Test "suc" [nock| [0 1] |] [nock| [suc [quote 1]] |] (eqNock [nock| 2 |]),
    Test "eq" [nock| [0 1] |] [nock| [= [1 0] [1 0]] |] (eqNock [nock| true |]),
    Test "eq" [nock| [0 1] |] [nock| [= [1 0] [0 1]] |] (eqNock [nock| false |]),
    Test "if" [nock| [0 1] |] [nock| [if [quote true] [@ L] [@ R]] |] (eqNock [nock| 0 |]),
    Test "if" [nock| [0 1] |] [nock| [if [quote false] [@ L] [@ R]] |] (eqNock [nock| 1 |]),
    Test "seq" [nock| [0 1] |] [nock| [seq [[suc [@ L]] [@ R]] [suc [@ L]]] |] (eqNock [nock| 2 |]),
    Test "push" [nock| [0 1] |] [nock| [push [[suc [@ L]] [@ S]]] |] (eqNock [nock| [1 0 1] |]),
    Test "call" [nock| [quote 1] |] [nock| [call [S [@ S]]] |] (eqNock [nock| 1 |]),
    Test "replace" [nock| [0 1] |] [nock| [replace [[L [quote 1]] [@ S]]] |] (eqNock [nock| [1 1] |]),
    Test "hint" [nock| [0 1] |] [nock| [hint [nil [trace [quote 2] [quote 3]]] [quote 1]] |] (eqTraces [[nock| 2 |]] >> eqNock [nock| 1 |])
  ]
