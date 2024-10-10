module Nockma.Eval.Positive where

import Base hiding (Path, testName)
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Language.Base (defaultSymbol)
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib (stdlib)
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Compiler.Nockma.Translation.FromTree

type Check = Sem '[Reader [Term Natural], Reader (Term Natural), EmbedIO]

data Test = Test
  { _testEvalOptions :: EvalOptions,
    _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion),
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
    Right r -> case _testAssertEvalError of
      Nothing -> case r of
        Left evalErr -> assertFailure ("Evaluation error: " <> unpack (ppTrace evalErr))
        Right res -> runM (runReader res (runReader traces _testCheck))
      Just checkErrFn -> case r of
        Left evalErr -> checkErrFn evalErr
        Right {} -> assertFailure "expected error"

allTests :: TestTree
allTests =
  testGroup
    "Nockma eval positive"
    [ testGroup "Unit" (map mkNockmaTest unitTests),
      testGroup "Juvix calling convention" (map mkNockmaTest juvixCallingConventionTests),
      testGroup "Anoma calling convention" (map mkNockmaTest anomaCallingConventionTests),
      testGroup "Anoma serialization tests" (map mkNockmaTest serializationTests)
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
  anomaTest n mainFun [] _testCheck _evalInterceptStdlibCalls

compilerTestM :: Text -> Sem '[Reader CompilerCtx] (Term Natural) -> Check () -> Bool -> Test
compilerTestM n mainFun =
  compilerTest n (run . runReader emptyCompilerCtx $ mainFun)

serializationTest :: Term Natural -> Term Natural -> [Test]
serializationTest jamTerm cueTerm = run . runReader emptyCompilerCtx $ do
  let jamCheck :: Check () = eqNock cueTerm
  jamCall :: Term Natural <- callStdlib StdlibEncode [OpQuote # jamTerm]
  cueCall :: Term Natural <- callStdlib StdlibDecode [OpQuote # cueTerm]
  let cueCheck :: Check () = eqNock jamTerm
      ppJamTerm :: Text = ppPrint jamTerm
      ppCueTerm :: Text = ppPrint cueTerm
  return
    [ anomaTest ("jam " <> ppJamTerm <> " == " <> ppCueTerm) jamCall [] jamCheck True,
      anomaTest ("cue " <> ppCueTerm <> " == " <> ppJamTerm) cueCall [] cueCheck True
    ]

-- | Test decode (encode t) = t
serializationIdTest :: Text -> Term Natural -> Test
serializationIdTest n jamTerm = run . runReader emptyCompilerCtx $ do
  arg <- callStdlib StdlibEncode [OpQuote # jamTerm]
  call <- callStdlib StdlibDecode [arg]
  return $ anomaTest (n <> ": " <> "cue . jam = id") call [] (eqNock jamTerm) True

withAssertErrKeyNotInStorage :: Test -> Test
withAssertErrKeyNotInStorage Test {..} =
  let _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion) = Just f
   in Test {..}
  where
    f :: NockEvalError Natural -> Assertion
    f = \case
      ErrKeyNotInStorage {} -> return ()
      _ -> assertFailure "Expected ErrKeyNotInStorage error"

anomaTest :: Text -> Term Natural -> [Term Natural] -> Check () -> Bool -> Test
anomaTest n mainFun args _testCheck _evalInterceptStdlibCalls =
  let f =
        CompilerFunction
          { _compilerFunctionId = UserFunction (defaultSymbol 0),
            _compilerFunctionArity = fromIntegral (length args),
            _compilerFunction = return mainFun,
            _compilerFunctionName = "main"
          }
      _testName :: Text
        | _evalInterceptStdlibCalls = n <> " - intercept stdlib"
        | otherwise = n

      opts = CompilerOptions

      res :: AnomaResult = runCompilerWith opts mempty [] f
      _testProgramSubject = res ^. anomaClosure

      _testProgramFormula = anomaCall args
      _testProgramStorage :: Storage Natural = emptyStorage
      _testEvalOptions = EvalOptions {..}
      _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion) = Nothing
   in Test {..}

anomaTestM :: Text -> Sem '[Reader CompilerCtx] (Term Natural) -> [Term Natural] -> Check () -> Bool -> Test
anomaTestM n mainFun =
  anomaTest n (run . runReader emptyCompilerCtx $ mainFun)

testWithStorage :: [(Term Natural, Term Natural)] -> Text -> Term Natural -> Term Natural -> Check () -> Test
testWithStorage s = Test defaultEvalOptions Nothing (Storage (HashMap.fromList (first StorageKey <$> s)))

test :: Text -> Term Natural -> Term Natural -> Check () -> Test
test = testWithStorage []

--- A Nock formula that computes logical and
nockAnd :: Term Natural
nockAnd = [nock| [6 [0 12] [6 [0 13] [1 0] [1 1]] [1 1]] |]

--- A nock function that computes logical and
nockAndArgs :: Term Natural
nockAndArgs = nockAnd # args # env
  where
    arg :: Term Natural
    arg = nockNilTagged "placeholder argument"

    args :: Term Natural
    args = arg # arg

    env :: Term Natural
    env = nockNilTagged "environment"

-- | Wrap a function in a formula that calls the function with arguments from the subject.
applyFun :: Term Natural -> Term Natural
applyFun f =
  OpPush
    # f
    # ( OpCall
          # codePath
          # ( OpReplace
                # ( argsPath
                      # (OpAddress # subjectPath >># (OpAddress # argsPath))
                  )
                # (OpAddress # fPath)
            )
      )
  where
    codePath :: Path
    codePath = closurePath FunCode

    argsPath :: Path
    argsPath = closurePath ArgsTuple

    -- Path to the function being applied after pushing
    fPath :: Path
    fPath = [L]

    -- Path to the original subject after pushing
    subjectPath :: Path
    subjectPath = [R]

anomaCallingConventionTests :: [Test]
anomaCallingConventionTests =
  [True, False]
    <**> [ anomaTestM "stdlib add" (add (nockNatLiteral 1) (nockNatLiteral 2)) [] (eqNock [nock| 3 |]),
           anomaTestM "stdlib add with arg" (add (nockNatLiteral 1) (nockNatLiteral 2)) [nockNatLiteral 1] (eqNock [nock| 3 |]),
           let args = [nockNatLiteral 3, nockNatLiteral 1]
               fx =
                 FunctionCtx
                   { _functionCtxArity = fromIntegral (length args)
                   }
            in run . runReader fx . runReader emptyCompilerCtx $ do
                 p0 <- pathToArg 0
                 p1 <- pathToArg 1
                 return (anomaTestM "stdlib sub args" (sub (OpAddress # p0) (OpAddress # p1)) args (eqNock [nock| 2 |])),
           --- sanity check nockAnd
           anomaTestM "(and true false) == false" (return nockAnd) [nockTrueLiteral, nockFalseLiteral] (eqNock [nock| false |]),
           anomaTestM "(and true true) == true" (return nockAnd) [nockTrueLiteral, nockTrueLiteral] (eqNock [nock| true |]),
           --- test curry with and
           anomaTestM "(curry and true) false == false" (applyFun <$> stdlibCurry (OpQuote # nockAndArgs) nockTrueLiteral) [nockFalseLiteral] (eqNock [nock| false |]),
           anomaTestM "(curry and true) true == true" (applyFun <$> stdlibCurry (OpQuote # nockAndArgs) nockTrueLiteral) [nockTrueLiteral] (eqNock [nock| true |]),
           anomaTestM "(curry and false) true == false" (applyFun <$> stdlibCurry (OpQuote # nockAndArgs) nockFalseLiteral) [nockTrueLiteral] (eqNock [nock| false |]),
           anomaTestM "(curry and false) false == false" (applyFun <$> stdlibCurry (OpQuote # nockAndArgs) nockFalseLiteral) [nockFalseLiteral] (eqNock [nock| false |])
         ]

serializationTests :: [Test]
serializationTests =
  serializationIdTest
    "stdlib"
    stdlib
    : serializationTest
      [nock| 0 |]
      [nock| 2 |]
      <> serializationTest
        [nock| 1 |]
        [nock| 12 |]
      <> serializationTest
        [nock| 2 |]
        [nock| 72 |]
      <> serializationTest
        [nock| 19 |]
        [nock| 2480 |]
      <> serializationTest
        [nock| 581.949.002 |]
        [nock| 1.191.831.557.952 |]
      <> serializationTest
        [nock| [0 19] |]
        [nock| 39689 |]
      <> serializationTest
        [nock| [1 1] |]
        [nock| 817 |]
      <> serializationTest
        [nock| [10.000 10.000] |]
        [nock| 4.952.983.169 |]
      <> serializationTest
        [nock| [999.999.999 999.999.999] |]
        [nock| 1.301.217.674.263.809 |]
      <> serializationTest
        [nock| [222 444 888] |]
        [nock| 250.038.217.192.960.129 |]
      <> serializationTest
        [nock| [[107 110] [107 110]] |]
        [nock| 635.080.761.093 |]
      <> serializationTest
        [nock| [0 1 2 3 4 5 6 7 8 9 10] |]
        [nock| 25.681.224.503.728.653.597.984.370.231.065 |]
      <> serializationTest
        [nock| [99 100 101 102 103 104 0] |]
        [nock| 223.372.995.869.285.333.705.242.560.449 |]
      <> serializationTest
        [nock| [[222 444 888] [222 444 888]] |]
        [nock| 170.479.614.045.978.345.989 |]
      <> serializationTest
        [nock| [[0 1] [1 2] [2 3] [3 4] 0] |]
        [nock| 11.976.248.475.217.237.797 |]
      <> serializationTest
        [nock| [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6] [6 7] [7 8] [8 9] 0] |]
        [nock| 7.694.087.033.387.855.647.747.387.855.514.468.399.947.749.137.782.565 |]
      <> serializationTest
        [nock| [[0 1] [2 3] [4 5] [6 7] [8 9] [10 11] [12 13] [14 15] [16 17] [18 19] [20 21] 0] |]
        [nock| 308.947.677.754.874.070.959.300.747.182.056.036.528.545.493.781.368.831.595.479.491.505.523.344.414.501 |]

-- Call a formula with specified arguments
nockCall :: Term Natural -> NonEmpty (Term Natural) -> Term Natural
nockCall formula args = (OpReplace # ([R, L] # foldTerms args) # (OpQuote # formula)) >># (OpCall # [L] # (OpAddress # emptyPath))

juvixCallingConventionTests :: [Test]
juvixCallingConventionTests =
  [True, False]
    <**> [ compilerTestM "stdlib add" (add (nockNatLiteral 1) (nockNatLiteral 2)) (eqNock [nock| 3 |]),
           compilerTestM "stdlib dec" (dec (nockNatLiteral 1)) (eqNock [nock| 0 |]),
           compilerTestM "stdlib mul" (mul (nockNatLiteral 2) (nockNatLiteral 3)) (eqNock [nock| 6 |]),
           compilerTestM "stdlib sub" (sub (nockNatLiteral 2) (nockNatLiteral 1)) (eqNock [nock| 1 |]),
           compilerTestM "stdlib div" (callStdlib StdlibDiv [nockNatLiteral 10, nockNatLiteral 3]) (eqNock [nock| 3 |]),
           compilerTestM "stdlib mod" (callStdlib StdlibMod [nockNatLiteral 3, nockNatLiteral 2]) (eqNock [nock| 1 |]),
           compilerTestM "stdlib le" (callStdlib StdlibLe [nockNatLiteral 3, nockNatLiteral 3]) (eqNock [nock| true |]),
           compilerTestM "stdlib lt" (callStdlib StdlibLt [nockNatLiteral 3, nockNatLiteral 3]) (eqNock [nock| false |]),
           compilerTestM "stdlib pow2" (pow2 (nockNatLiteral 3)) (eqNock [nock| 8 |]),
           compilerTestM "stdlib nested" (dec =<< (dec (nockNatLiteral 20))) (eqNock [nock| 18 |]),
           compilerTestM "append rights - empty" (appendRights emptyPath (nockNatLiteral 3)) (eqNock (toNock [R, R, R])),
           compilerTestM "append rights" (appendRights [L, L] (nockNatLiteral 3)) (eqNock (toNock [L, L, R, R, R])),
           compilerTest "opAddress" ((OpQuote # (foldTerms (toNock @Natural <$> (5 :| [6, 1])))) >># opAddress' (OpQuote # [R, R])) (eqNock (toNock @Natural 1)),
           compilerTest "foldTermsOrNil (empty)" (foldTermsOrNil []) (eqNock (nockNilTagged "expected-result")),
           let l :: NonEmpty Natural = 1 :| [2]
               l' :: NonEmpty (Term Natural) = nockNatLiteral <$> l
            in compilerTest "foldTermsOrNil (non-empty)" (foldTermsOrNil (toList l')) (eqNock (foldTerms (toNock @Natural <$> l))),
           let l :: Term Natural = OpQuote # foldTerms (toNock @Natural <$> (1 :| [2, 3]))
            in compilerTest "replaceSubterm'" (replaceSubterm' l (OpQuote # toNock [R]) (OpQuote # (toNock @Natural 999))) (eqNock (toNock @Natural 1 # toNock @Natural 999)),
           let lst :: [Term Natural] = toNock @Natural <$> [1, 2, 3]
               len = nockIntegralLiteral (length lst)
               l :: Term Natural = OpQuote # makeList lst
            in compilerTestM "append" (append l len l) (eqNock (makeList (lst ++ lst))),
           let l :: [Natural] = [1, 2]
               r :: NonEmpty Natural = 3 :| [4]
               res :: Term Natural = foldTerms (toNock <$> prependList l r)
               lenL :: Term Natural = nockIntegralLiteral (length l)
               lstL = OpQuote # makeList (map toNock l)
               tupR = OpQuote # foldTerms (toNock <$> r)
            in compilerTestM "appendToTuple (left non-empty, right non-empty)" (appendToTuple lstL lenL tupR) (eqNock res),
           let r :: NonEmpty Natural = 3 :| [4]
               res :: Term Natural = foldTerms (toNock <$> r)
               tupR = OpQuote # foldTerms (toNock <$> r)
            in compilerTestM "appendToTuple (left empty, right-nonempty)" (appendToTuple (OpQuote # nockNilTagged "test-appendtotuple") (nockNatLiteral 0) tupR) (eqNock res),
           compilerTestM "stdlib cat" (callStdlib StdlibCatBytes [nockNatLiteral 2, nockNatLiteral 1]) (eqNock [nock| 258 |]),
           compilerTestM "fold bytes empty" (callStdlib StdlibFoldBytes [OpQuote # makeList []]) (eqNock [nock| 0 |]),
           compilerTestM "fold bytes [1, 0, 0] == 1" (callStdlib StdlibFoldBytes [OpQuote # makeList (toNock @Natural <$> [1, 0, 0])]) (eqNock [nock| 1 |]),
           compilerTestM "fold bytes single byte" (callStdlib StdlibFoldBytes [OpQuote # makeList (toNock @Natural <$> [123])]) (eqNock [nock| 123 |]),
           compilerTestM "fold bytes [0, 1] == 256" (callStdlib StdlibFoldBytes [OpQuote # makeList (toNock @Natural <$> [0, 1])]) (eqNock [nock| 256 |]),
           compilerTestM "fold bytes [5, 1] == 261" (callStdlib StdlibFoldBytes [OpQuote # makeList (toNock @Natural <$> [5, 1])]) (eqNock [nock| 261 |]),
           compilerTestM "fold bytes [0, 1, 0] == 256" (callStdlib StdlibFoldBytes [OpQuote # makeList (toNock @Natural <$> [0, 1, 0])]) (eqNock [nock| 256 |]),
           compilerTestM "length [] == 0" (callStdlib StdlibLengthList [OpQuote # makeList []]) (eqNock [nock| 0 |]),
           compilerTestM "length [10] == 1" (callStdlib StdlibLengthList [OpQuote # makeList [[nock| 10 |]]]) (eqNock [nock| 1 |]),
           compilerTestM "length [[1 2, 3], 0] == 2" (callStdlib StdlibLengthList [OpQuote # makeList [[nock| [1 2 3] |], [nock| 0 |]]]) (eqNock [nock| 2 |]),
           compilerTestM "length-bytes 256 == 2" (callStdlib StdlibLengthBytes [nockNatLiteral 256]) (eqNock [nock| 2 |]),
           compilerTestM "length-bytes 255 == 1" (callStdlib StdlibLengthBytes [nockNatLiteral 255]) (eqNock [nock| 1 |]),
           compilerTestM "length-bytes 1 == 1" (callStdlib StdlibLengthBytes [nockNatLiteral 1]) (eqNock [nock| 1 |]),
           compilerTestM "length-bytes 0 == 0" (callStdlib StdlibLengthBytes [nockNatLiteral 0]) (eqNock [nock| 0 |])
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
    test "hint" [nock| [0 1] |] [nock| [hint [1937012080 [quote 2]] [quote 1]] |] (eqTraces [[nock| 2 |]] >> eqNock [nock| 1 |]),
    testWithStorage [([nock| 111 |], [nock| 222 |])] "scry" [nock| nil |] [nock| [scry [quote nil] [quote 111]] |] (eqNock [nock| 222 |]),
    withAssertErrKeyNotInStorage $ testWithStorage [([nock| 333 |], [nock| 222 |]), ([nock| 3 |], [nock| 222 |])] "scry" [nock| nil |] [nock| [scry [quote nil] [quote 111]] |] (eqNock [nock| 222 |])
  ]
