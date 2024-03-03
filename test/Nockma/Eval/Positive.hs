module Nockma.Eval.Positive where

import Base hiding (Path, testName)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language.Base (defaultSymbol)
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Compiler.Nockma.Translation.FromTree
import Nockma.Base

type Check = Sem '[Reader [Term Natural], Reader (Term Natural), EmbedIO]

data Test = Test
  { _testEvalOptions :: EvalOptions,
    _testProgramStorage :: Storage Natural,
    _testName :: Text,
    _testProgramSubject :: Term Natural,
    _testProgramFormula :: Term Natural,
    _testCheck :: Check ()
  }

makeLenses ''Test

mkNockmaAssertion :: Test -> Assertion
mkNockmaAssertion Test {..} = do
  let (traces, evalResult) =
        run
          . runReader _testEvalOptions
          . runOutputList @(Term Natural)
          . runError @(ErrNockNatural Natural)
          . runError @(NockEvalError Natural)
          . runReader @(Storage Natural) _testProgramStorage
          $ eval _testProgramSubject _testProgramFormula

  case evalResult of
    Left natErr -> assertFailure ("Evaluation error: " <> show natErr)
    Right r -> case r of
      Left evalErr -> assertFailure ("Evaluation error: " <> unpack (ppTrace evalErr))
      Right res -> runM (runReader res (runReader traces _testCheck))

allTests :: TestTree
allTests =
  testGroup
    "Nockma eval positive"
    [ testGroup "Unit" (map mkNockmaTest unitTests),
      testGroup "Juvix calling convention" (map mkNockmaTest juvixCallingConventionTests),
      testGroup "Anoma calling convention" (map mkNockmaTest anomaCallingConventionTests)
    ]
  where
    mkNockmaTest :: Test -> TestTree
    mkNockmaTest t = testCase (unpack (t ^. testName)) (mkNockmaAssertion t)

eqNock :: Term Natural -> Check ()
eqNock expected = do
  actual <- ask
  unless (nockmaEq expected actual) (err actual)
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
  unless (nockmaEq ts expected) (err ts)
  where
    err :: [Term Natural] -> Check ()
    err ts = do
      let msg =
            "Expected traces:\n"
              <> ppTrace expected
              <> "\nBut got:\n"
              <> ppTrace ts
      assertFailure (unpack msg)

compilerTest :: Text -> Term Natural -> Check () -> Bool -> Test
compilerTest n mainFun _testCheck _evalInterceptStdlibCalls =
  let f =
        CompilerFunction
          { _compilerFunctionName = UserFunction (defaultSymbol 0),
            _compilerFunctionArity = 0,
            _compilerFunction = return mainFun
          }
      _testName :: Text
        | _evalInterceptStdlibCalls = n <> " - intercept stdlib"
        | otherwise = n
      opts = CompilerOptions {_compilerOptionsEnableTrace = False}
      Cell _testProgramSubject _testProgramFormula = runCompilerWithJuvix opts mempty [] f
      _testEvalOptions = EvalOptions {..}
      _testProgramStorage :: Storage Natural = emptyStorage
   in Test {..}

anomaTest :: Text -> Term Natural -> [Term Natural] -> Check () -> Bool -> Test
anomaTest n mainFun args _testCheck _evalInterceptStdlibCalls =
  let f =
        CompilerFunction
          { _compilerFunctionName = UserFunction (defaultSymbol 0),
            _compilerFunctionArity = fromIntegral (length args),
            _compilerFunction = return mainFun
          }
      _testName :: Text
        | _evalInterceptStdlibCalls = n <> " - intercept stdlib"
        | otherwise = n

      opts = CompilerOptions {_compilerOptionsEnableTrace = False}

      _testProgramSubject = TermCell (runCompilerWithAnoma opts mempty [] f)

      _testProgramFormula = anomaCall args
      _testProgramStorage :: Storage Natural = emptyStorage
      _testEvalOptions = EvalOptions {..}
   in Test {..}

testWithStorage :: [(Term Natural, Term Natural)] -> Text -> Term Natural -> Term Natural -> Check () -> Test
testWithStorage s = Test defaultEvalOptions (Storage (HashMap.fromList s))

test :: Text -> Term Natural -> Term Natural -> Check () -> Test
test = testWithStorage []

anomaCallingConventionTests :: [Test]
anomaCallingConventionTests =
  [True, False]
    <**> [ anomaTest "stdlib add" (add (nockNatLiteral 1) (nockNatLiteral 2)) [] (eqNock [nock| 3 |]),
           anomaTest "stdlib add with arg" (add (nockNatLiteral 1) (nockNatLiteral 2)) [nockNatLiteral 1] (eqNock [nock| 3 |]),
           anomaTest "stdlib sub args" (sub (OpAddress # pathToArg 0) (OpAddress # pathToArg 1)) [nockNatLiteral 3, nockNatLiteral 1] (eqNock [nock| 2 |])
         ]

juvixCallingConventionTests :: [Test]
juvixCallingConventionTests =
  [True, False]
    <**> [ compilerTest "stdlib add" (add (nockNatLiteral 1) (nockNatLiteral 2)) (eqNock [nock| 3 |]),
           compilerTest "stdlib dec" (dec (nockNatLiteral 1)) (eqNock [nock| 0 |]),
           compilerTest "stdlib mul" (mul (nockNatLiteral 2) (nockNatLiteral 3)) (eqNock [nock| 6 |]),
           compilerTest "stdlib sub" (sub (nockNatLiteral 2) (nockNatLiteral 1)) (eqNock [nock| 1 |]),
           compilerTest "stdlib div" (callStdlib StdlibDiv [nockNatLiteral 10, nockNatLiteral 3]) (eqNock [nock| 3 |]),
           compilerTest "stdlib mod" (callStdlib StdlibMod [nockNatLiteral 3, nockNatLiteral 2]) (eqNock [nock| 1 |]),
           compilerTest "stdlib le" (callStdlib StdlibLe [nockNatLiteral 3, nockNatLiteral 3]) (eqNock [nock| true |]),
           compilerTest "stdlib lt" (callStdlib StdlibLt [nockNatLiteral 3, nockNatLiteral 3]) (eqNock [nock| false |]),
           compilerTest "stdlib pow2" (pow2 (nockNatLiteral 3)) (eqNock [nock| 8 |]),
           compilerTest "stdlib nested" (dec (dec (nockNatLiteral 20))) (eqNock [nock| 18 |]),
           compilerTest "append rights - empty" (appendRights emptyPath (nockNatLiteral 3)) (eqNock (toNock [R, R, R])),
           compilerTest "append rights" (appendRights [L, L] (nockNatLiteral 3)) (eqNock (toNock [L, L, R, R, R]))
         ]

unitTests :: [Test]
unitTests =
  [ test "address" [nock| [0 1] |] [nock| [[@ R] [@ L]] |] (eqNock [nock| [1 0] |]),
    test "address nested" [nock| [0 1 2 3 4 5] |] [nock| [@ RRRRR] |] (eqNock [nock| 5 |]),
    test "quote" [nock| [0 1] |] [nock| [quote [1 0]] |] (eqNock [nock| [1 0] |]),
    test "apply" [nock| [0 1] |] [nock| [apply [@ S] [quote [@ R]]] |] (eqNock [nock| 1 |]),
    test "isCell atom" [nock| [0 1] |] [nock| [isCell [@ L]] |] (eqNock [nock| false |]),
    test "isCell cell" [nock| [0 1] |] [nock| [isCell [@ S]] |] (eqNock [nock| true |]),
    test "suc" [nock| [0 1] |] [nock| [suc [quote 1]] |] (eqNock [nock| 2 |]),
    test "eq" [nock| [0 1] |] [nock| [= [1 0] [1 0]] |] (eqNock [nock| true |]),
    test "eq" [nock| [0 1] |] [nock| [= [1 0] [0 1]] |] (eqNock [nock| false |]),
    test "if" [nock| [0 1] |] [nock| [if [quote true] [@ L] [@ R]] |] (eqNock [nock| 0 |]),
    test "if" [nock| [0 1] |] [nock| [if [quote false] [@ L] [@ R]] |] (eqNock [nock| 1 |]),
    test "seq" [nock| [0 1] |] [nock| [seq [[suc [@ L]] [@ R]] [suc [@ L]]] |] (eqNock [nock| 2 |]),
    test "push" [nock| [0 1] |] [nock| [push [[suc [@ L]] [@ S]]] |] (eqNock [nock| [1 0 1] |]),
    test "call" [nock| [quote 1] |] [nock| [call [S [@ S]]] |] (eqNock [nock| 1 |]),
    test "replace" [nock| [0 1] |] [nock| [replace [[L [quote 1]] [@ S]]] |] (eqNock [nock| [1 1] |]),
    test "hint" [nock| [0 1] |] [nock| [hint [nil [trace [quote 2] [quote 3]]] [quote 1]] |] (eqTraces [[nock| 2 |]] >> eqNock [nock| 1 |]),
    testWithStorage [([nock| 111 |], [nock| 222 |])] "scry" [nock| nil |] [nock| [scry [quote nil] [quote 111]] |] (eqNock [nock| 222 |])
  ]
