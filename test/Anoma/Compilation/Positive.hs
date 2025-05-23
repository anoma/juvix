module Anoma.Compilation.Positive (allTests) where

import Anoma.Effect.Base
import Anoma.Effect.RunNockma
import Base
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Data.Module
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Prelude qualified as Prelude
import Nockma.Eval.Positive (Check, Test (..), eqNock, eqTraces)
import Nockma.Eval.Positive qualified as NockmaEval

data AnomaTestMode
  = -- | We only run the Haskell evaluator tests in debug mode, where calls to stdlib are intercepted
    AnomaTestModeDebugOnly
  | -- | We run the Haskell tests in debug mode and in non-debug mode, where no stdlib
    -- calls are intercepted. Only tests that run fast can use this mode
    AnomaTestModeFull
  | -- | We run the test on the node only.
    -- Use this for tests that must be jetted on the node.
    AnomaTestModeNodeOnly

data AnomaTest = AnomaTest
  { _anomaTestMode :: AnomaTestMode,
    _anomaProgramStorage :: Storage Natural,
    _anomaTestNum :: Int,
    _anomaTestTag :: Text,
    _anomaRelRoot :: Prelude.Path Rel Dir,
    _anomaMainFile :: Prelude.Path Rel File,
    _anomaArgs :: [Term Natural],
    _anomaCheck :: Check ()
  }

root :: Prelude.Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Compilation/positive")

anomaTestName :: AnomaTest -> Text
anomaTestName AnomaTest {..} = numberedTestName _anomaTestNum _anomaTestTag

fromAnomaTest :: AnomaTest -> Maybe TestTree
fromAnomaTest a@AnomaTest {..} =
  let testNonDebug = mkTest' False
      testDebug = mkTest' True
   in case _anomaTestMode of
        AnomaTestModeNodeOnly -> Nothing
        AnomaTestModeDebugOnly -> Just (testGroup (unpack baseTestname) [testDebug])
        AnomaTestModeFull -> Just (testGroup (unpack baseTestname) [testDebug, testNonDebug])
  where
    baseTestname :: Text
    baseTestname = anomaTestName a

    mkTest' :: Bool -> TestTree
    mkTest' enableDebug =
      let testName' :: Text
            | enableDebug = baseTestname <> " debug"
            | otherwise = baseTestname <> " non-debug"
          tIO :: IO Test = do
            (mid, mtab) <- withRootCopy (compileAnomaModular enableDebug _anomaRelRoot _anomaMainFile)
            let ms = mkModuleStorage mtab
                md = lookupModuleTable mtab mid
                _testProgramFormula = anomaCall (map (opQuote "Quote arg") _anomaArgs)
                _testProgramSubject = getModuleCode md
                _testEvalOptions = defaultEvalOptions
                _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion) = Nothing
            return
              Test
                { _testCheck = _anomaCheck,
                  _testProgramStorage = _anomaProgramStorage <> ms,
                  _testName = testName',
                  ..
                }
       in testCase (unpack testName') (tIO >>= NockmaEval.mkNockmaAssertion)

mkAnomaTest' ::
  AnomaTestMode ->
  Storage Natural ->
  Int ->
  Text ->
  Prelude.Path Rel Dir ->
  Prelude.Path Rel File ->
  [Term Natural] ->
  Check () ->
  AnomaTest
mkAnomaTest' _anomaTestMode _anomaProgramStorage _anomaTestNum _anomaTestTag _anomaRelRoot _anomaMainFile _anomaArgs _anomaCheck =
  AnomaTest
    { ..
    }

mkAnomaNodeTest :: AnomaTest -> TestTree
mkAnomaNodeTest a@AnomaTest {..} =
  testCase (anomaTestName a <> " - node") assertion
  where
    assertion :: Assertion
    assertion = do
      program :: Term Natural <- withRootCopy (compileAnomaMain False _anomaRelRoot _anomaMainFile)
      testAnomaPath <- envAnomaPath
      runM
        . ignoreLogger
        . runSimpleErrorHUnit
        . runAnomaEphemeral testAnomaPath
        $ do
          let rinput =
                RunNockmaInput
                  { _runNockmaProgram = program,
                    _runNockmaArgs = map RunNockmaArgTerm _anomaArgs
                  }
          out <- runNockma rinput
          runM
            . runReader (out ^. runNockmaResult)
            . runReader (out ^. runNockmaTraces)
            $ _anomaCheck

withRootCopy :: (Prelude.Path Abs Dir -> IO a) -> IO a
withRootCopy = withRootTmpCopy root

mkAnomaTest ::
  Int ->
  AnomaTestMode ->
  Text ->
  Prelude.Path Rel Dir ->
  Prelude.Path Rel File ->
  [Term Natural] ->
  Check () ->
  AnomaTest
mkAnomaTest testNum testMode testName' dirPath filePath args check =
  mkAnomaTest' testMode emptyStorage testNum testName' dirPath filePath args check

checkNatOutput :: [Natural] -> Check ()
checkNatOutput = checkTracesAndOutput . fmap toNock

-- | The expected result of the program is the last item in the list. If the
-- list is empty then it is expected to return void
checkTracesAndOutput :: [Term Natural] -> Check ()
checkTracesAndOutput expected = case unsnoc expected of
  Nothing -> eqTraces [] >> eqNock (TermAtom nockVoid)
  Just (xs, x) -> do
    eqTraces xs
    eqNock x

data TestClass
  = ClassWorking
  | -- | The anoma node returns a response with an error
    ClassNodeError
  | -- | The anoma node returns a value but it doesn't match the expected value
    ClassWrong
  | -- | We have no test with this number
    ClassMissing
  | ClassExpectedFail
  deriving stock (Eq, Show)

classify :: AnomaTest -> TestClass
classify AnomaTest {..} = case _anomaTestNum of
  1 -> ClassWorking
  2 -> ClassWorking
  3 -> ClassWorking
  4 -> ClassMissing
  5 -> ClassWorking
  6 -> ClassWorking
  7 -> ClassWorking
  8 -> ClassWorking
  9 -> ClassWorking
  10 -> ClassWorking
  11 -> ClassWorking
  12 -> ClassWorking
  13 -> ClassWorking
  14 -> ClassWorking
  15 -> ClassWorking
  16 -> ClassWorking
  17 -> ClassWorking
  18 -> ClassWorking
  19 -> ClassWorking
  20 -> ClassWorking
  21 -> ClassWorking
  22 -> ClassWorking
  23 -> ClassWorking
  24 -> ClassWorking
  25 -> ClassWorking
  26 -> ClassWorking
  27 -> ClassMissing
  28 -> ClassWorking
  29 -> ClassWorking
  30 -> ClassWorking
  31 -> ClassWorking
  32 -> ClassWorking
  33 -> ClassWorking
  34 -> ClassWorking
  35 -> ClassWorking
  36 -> ClassWorking
  37 -> ClassWorking
  38 -> ClassWorking
  39 -> ClassWorking
  40 -> ClassWorking
  41 -> ClassWorking
  42 -> ClassMissing
  43 -> ClassWorking
  45 -> ClassWorking
  46 -> ClassWorking
  47 -> ClassWorking
  48 -> ClassMissing
  49 -> ClassWorking
  50 -> ClassWorking
  51 -> ClassMissing
  52 -> ClassWorking
  53 -> ClassWorking
  54 -> ClassWorking
  55 -> ClassWorking
  56 -> ClassWorking
  57 -> ClassWorking
  58 -> ClassWorking
  59 -> ClassWorking
  60 -> ClassWorking
  61 -> ClassWorking
  62 -> ClassWorking
  63 -> ClassWorking
  64 -> ClassWorking
  65 -> ClassWorking
  66 -> ClassWorking
  67 -> ClassWorking
  68 -> ClassWorking
  69 -> ClassWorking
  70 -> ClassWorking
  71 -> ClassWorking
  72 -> ClassWorking
  73 -> ClassWorking
  74 -> ClassExpectedFail
  75 -> ClassWorking
  76 -> ClassWorking
  77 -> ClassWorking
  78 -> ClassWorking
  79 -> ClassWorking
  80 -> ClassWorking
  81 -> ClassWorking
  82 -> ClassWorking
  83 -> ClassWorking
  84 -> ClassWorking
  85 -> ClassWorking
  86 -> ClassExpectedFail
  87 -> ClassWorking
  88 -> ClassWorking
  89 -> ClassWorking
  90 -> ClassWorking
  _ -> error "non-exhaustive test classification"

allTests :: TestTree
allTests =
  testGroup
    "Anoma positive tests"
    [ haskellNockmaTests,
      anomaNodeTests
    ]
  where
    anomaNodeTests :: TestTree
    anomaNodeTests =
      sequentialTestGroup
        "AnomaNode"
        AllFinish
        (map mkAnomaNodeTest (filter shouldRun anomaTests))
      where
        shouldRun :: AnomaTest -> Bool
        shouldRun a = classify a == ClassWorking

    haskellNockmaTests :: TestTree
    haskellNockmaTests =
      testGroup
        "Anoma positive tests (Haskell evaluator)"
        (mapMaybe fromAnomaTest anomaTests)

    natArg :: Natural -> Term Natural
    natArg = toNock

    anomaTests :: [AnomaTest]
    anomaTests =
      [ mkAnomaTest
          1
          AnomaTestModeFull
          "Arithmetic operators"
          $(mkRelDir ".")
          $(mkRelFile "test001.juvix")
          [natArg 5]
          (checkNatOutput [11]),
        mkAnomaTest
          2
          AnomaTestModeFull
          "Arithmetic operators inside lambdas"
          $(mkRelDir ".")
          $(mkRelFile "test002.juvix")
          [natArg 2]
          (checkNatOutput [11]),
        mkAnomaTest
          3
          AnomaTestModeFull
          "Integer arithmetic"
          $(mkRelDir ".")
          $(mkRelFile "test003.juvix")
          []
          (checkNatOutput [1, 4, 2, 4, 0]),
        mkAnomaTest
          5
          AnomaTestModeFull
          "Higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test005.juvix")
          [natArg 1]
          (checkNatOutput [6]),
        mkAnomaTest
          6
          AnomaTestModeFull
          "If-then-else and lazy boolean operators"
          $(mkRelDir ".")
          $(mkRelFile "test006.juvix")
          []
          (checkTracesAndOutput [[nock| 2 |], [nock| true |], [nock| false |]]),
        mkAnomaTest
          7
          AnomaTestModeFull
          "Pattern matching and lambda-case"
          $(mkRelDir ".")
          $(mkRelFile "test007.juvix")
          []
          $ do
            let l :: Term Natural = [nock| [1 2 nil] |]
            checkTracesAndOutput [[nock| false |], [nock| true |], [nock| 0 |], [nock| [1 nil] |], [nock| 1 |], l, l],
        mkAnomaTest
          8
          AnomaTestModeDebugOnly
          "Recursion"
          $(mkRelDir ".")
          $(mkRelFile "test008.juvix")
          [natArg 1000]
          (eqNock [nock| 500500 |]),
        mkAnomaTest
          9
          AnomaTestModeDebugOnly
          "Tail recursion"
          $(mkRelDir ".")
          $(mkRelFile "test009.juvix")
          [natArg 1000]
          $ checkNatOutput [500500, 120, 3628800, 479001600],
        mkAnomaTest
          10
          AnomaTestModeFull
          "Let"
          $(mkRelDir ".")
          $(mkRelFile "test010.juvix")
          []
          (checkNatOutput [32]),
        mkAnomaTest
          11
          AnomaTestModeDebugOnly
          "Tail recursion: Fibonacci numbers in linear time"
          $(mkRelDir ".")
          $(mkRelFile "test011.juvix")
          []
          $ do
            let fib10 :: Natural = 55
                fib100 :: Natural = 354224848179261915075
                fib1000 :: Natural = 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
            checkNatOutput [fib10, fib100, fib1000],
        mkAnomaTest
          12
          AnomaTestModeDebugOnly
          "Trees"
          $(mkRelDir ".")
          $(mkRelFile "test012.juvix")
          [natArg 1000]
          $ checkNatOutput
            [ 13200200200,
              21320020020013200200200,
              3213200200200132002002002132002002001320020020021320020020013200200200,
              13213200200200132002002002132002002001320020020021320020020013200200200,
              21321320020020013200200200213200200200132002002002132002002001320020020013213200200200132002002002132002002001320020020021320020020013200200200
            ],
        mkAnomaTest
          13
          AnomaTestModeFull
          "Functions returning functions with variable capture"
          $(mkRelDir ".")
          $(mkRelFile "test013.juvix")
          []
          $ checkNatOutput [1, 0, 2, 5],
        mkAnomaTest
          14
          AnomaTestModeFull
          "Arithmetic"
          $(mkRelDir ".")
          $(mkRelFile "test014.juvix")
          []
          $ checkNatOutput [7, 17, 37, 31],
        mkAnomaTest
          15
          AnomaTestModeFull
          "Local functions with free variables"
          $(mkRelDir ".")
          $(mkRelFile "test015.juvix")
          []
          $ checkNatOutput [600, 25, 30, 45, 55, 16],
        mkAnomaTest
          16
          AnomaTestModeFull
          "Recursion through higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test016.juvix")
          []
          $ checkNatOutput [55],
        mkAnomaTest
          17
          AnomaTestModeDebugOnly
          "Tail recursion through higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test017.juvix")
          [natArg 1000]
          $ checkNatOutput [500500],
        mkAnomaTest
          18
          AnomaTestModeFull
          "Higher-order functions and recursion"
          $(mkRelDir ".")
          $(mkRelFile "test018.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          19
          AnomaTestModeFull
          "Self-application"
          $(mkRelDir ".")
          $(mkRelFile "test019.juvix")
          []
          $ checkNatOutput [7],
        mkAnomaTest
          20
          AnomaTestModeDebugOnly
          "Recursive functions: McCarthy's 91 function, subtraction by increments"
          $(mkRelDir ".")
          $(mkRelFile "test020.juvix")
          []
          $ checkNatOutput [91, 91, 91, 91, 100, 6, 6, 400, 4000],
        mkAnomaTest
          21
          AnomaTestModeDebugOnly
          "Fast exponentiation"
          $(mkRelDir ".")
          $(mkRelFile "test021.juvix")
          []
          $ checkNatOutput [8, 2187, 48828125],
        mkAnomaTest
          22
          AnomaTestModeDebugOnly
          "Lists"
          $(mkRelDir ".")
          $(mkRelFile "test022.juvix")
          [natArg 1000]
          $ checkTracesAndOutput
            [ [nock| [10 9 8 7 6 5 4 3 2 1 nil] |],
              [nock| [1 2 3 4 5 6 7 8 9 10 nil] |],
              [nock| [10 9 8 7 6 nil] |],
              [nock| [0 1 2 3 4 5 6 7 8 9 nil] |],
              [nock| 500500 |],
              [nock| 500500 |]
            ],
        mkAnomaTest
          23
          AnomaTestModeDebugOnly
          "Mutual recursion"
          $(mkRelDir ".")
          $(mkRelFile "test023.juvix")
          []
          $ checkNatOutput [32, 869, 6385109],
        mkAnomaTest
          24
          AnomaTestModeDebugOnly
          "Nested binders with variable capture"
          $(mkRelDir ".")
          $(mkRelFile "test024.juvix")
          []
          $ checkNatOutput [6688],
        mkAnomaTest
          25
          AnomaTestModeDebugOnly
          "Euclid's algorithm"
          $(mkRelDir ".")
          $(mkRelFile "test025.juvix")
          []
          $ checkNatOutput [14, 70, 1, 1, 1],
        mkAnomaTest
          26
          AnomaTestModeFull
          "Functional queues"
          $(mkRelDir ".")
          $(mkRelFile "test026.juvix")
          []
          $ checkTracesAndOutput [makeList (toNock @Natural <$> [1 .. 100])],
        -- TODO allow lambda branches of different number of patterns
        -- mkAnomaTest
        --   "Test027: Church numerals"
        -- AnomaTestModeFull
        --   $(mkRelDir ".")
        --   $(mkRelFile "test027.juvix")
        --   []
        --   $ checkNatOutput [7, 10, 21],
        mkAnomaTest
          28
          AnomaTestModeDebugOnly
          "Streams without memoization"
          $(mkRelDir ".")
          $(mkRelFile "test028.juvix")
          [natArg 10, natArg 30]
          $ checkNatOutput [31, 127],
        mkAnomaTest
          29
          AnomaTestModeFull
          "Ackermann function"
          $(mkRelDir ".")
          $(mkRelFile "test029.juvix")
          []
          $ checkNatOutput [8, 9, 15, 17, 29],
        mkAnomaTest
          30
          AnomaTestModeDebugOnly
          "Ackermann function (higher-order definition)"
          $(mkRelDir ".")
          $(mkRelFile "test030.juvix")
          []
          $ checkNatOutput [10, 21, 2187, 15],
        mkAnomaTest
          31
          AnomaTestModeFull
          "Nested lists"
          $(mkRelDir ".")
          $(mkRelFile "test031.juvix")
          []
          $ checkTracesAndOutput [[nock| [4 3 2 1 3 2 1 2 1 1 nil ] |]],
        mkAnomaTest
          32
          AnomaTestModeDebugOnly
          "Merge sort"
          $(mkRelDir ".")
          $(mkRelFile "test032.juvix")
          []
          $ do
            let l = makeList (toNock @Natural <$> [2 .. 11])
            checkTracesAndOutput [l, l, l],
        mkAnomaTest
          33
          AnomaTestModeFull
          "Eta-expansion of builtins and constructors"
          $(mkRelDir ".")
          $(mkRelFile "test033.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 9 |],
              [nock| [7 2] |],
              [nock| 5 |],
              [nock| [3 2] |],
              [nock| [1 2] |]
            ],
        mkAnomaTest
          34
          AnomaTestModeDebugOnly
          "Recursive let"
          $(mkRelDir ".")
          $(mkRelFile "test034.juvix")
          []
          $ checkNatOutput [500500, 32, 869, 41, 85],
        mkAnomaTest
          35
          AnomaTestModeDebugOnly
          "Pattern matching"
          $(mkRelDir ".")
          $(mkRelFile "test035.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| [9 7 5 3 1 nil] |],
              [nock| 300 |],
              [nock| 4160 |],
              [nock| 2336 |],
              [nock| 1 |],
              [nock| 0 |]
            ],
        mkAnomaTest
          36
          AnomaTestModeFull
          "Eta-expansion"
          $(mkRelDir ".")
          $(mkRelFile "test036.juvix")
          []
          $ checkNatOutput [18],
        mkAnomaTest
          37
          AnomaTestModeFull
          "Applications with lets and cases in function position"
          $(mkRelDir ".")
          $(mkRelFile "test037.juvix")
          []
          $ checkNatOutput [9],
        mkAnomaTest
          38
          AnomaTestModeFull
          "Simple case expression"
          $(mkRelDir ".")
          $(mkRelFile "test038.juvix")
          []
          $ checkNatOutput [1],
        mkAnomaTest
          39
          AnomaTestModeFull
          "Mutually recursive let expression"
          $(mkRelDir ".")
          $(mkRelFile "test039.juvix")
          []
          $ checkTracesAndOutput [[nock| false |], [nock| true |]],
        mkAnomaTest
          40
          AnomaTestModeFull
          "Pattern matching nullary constructor"
          $(mkRelDir ".")
          $(mkRelFile "test040.juvix")
          []
          $ checkTracesAndOutput [[nock| true |]],
        mkAnomaTest
          41
          AnomaTestModeFull
          "Use a builtin inductive in an inductive constructor"
          $(mkRelDir ".")
          $(mkRelFile "test041.juvix")
          []
          $ checkNatOutput [6],
        mkAnomaTest
          43
          AnomaTestModeFull
          "Builtin trace"
          $(mkRelDir ".")
          $(mkRelFile "test043.juvix")
          []
          $ checkNatOutput [0, 1],
        mkAnomaTest
          45
          AnomaTestModeFull
          "Implicit builtin bool"
          $(mkRelDir ".")
          $(mkRelFile "test045.juvix")
          []
          $ checkNatOutput [4],
        mkAnomaTest
          46
          AnomaTestModeFull
          "Polymorphic type arguments"
          $(mkRelDir ".")
          $(mkRelFile "test046.juvix")
          []
          $ checkNatOutput [7],
        mkAnomaTest
          47
          AnomaTestModeFull
          "Local Modules"
          $(mkRelDir ".")
          $(mkRelFile "test047.juvix")
          []
          $ checkNatOutput [660],
        mkAnomaTest
          49
          AnomaTestModeFull
          "Builtin Int"
          $(mkRelDir ".")
          $(mkRelFile "test049.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 1 |],
              [nock| 1 |],
              [nock| 0 |],
              [nock| 1|],
              [nock| 1 |],
              [nock| false |],
              [nock| 1|],
              [nock| 1 |],
              [nock| 4 |],
              [nock| true |],
              [nock| false |],
              [nock| false |],
              [nock| true |],
              [nock| true |],
              [nock| true |],
              [nock| 1|],
              [nock| 2|]
            ],
        mkAnomaTest
          50
          AnomaTestModeFull
          "Pattern matching with integers"
          $(mkRelDir ".")
          $(mkRelFile "test050.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          52
          AnomaTestModeFull
          "Simple lambda calculus"
          $(mkRelDir ".")
          $(mkRelFile "test052.juvix")
          []
          $ checkTracesAndOutput [[nock| [15 nil] |]],
        mkAnomaTest
          53
          AnomaTestModeFull
          "Inlining"
          $(mkRelDir ".")
          $(mkRelFile "test053.juvix")
          []
          $ checkNatOutput [21],
        mkAnomaTest
          54
          AnomaTestModeFull
          "Iterators"
          $(mkRelDir ".")
          $(mkRelFile "test054.juvix")
          []
          $ checkNatOutput [189],
        mkAnomaTest
          55
          AnomaTestModeFull
          "Constructor printing"
          $(mkRelDir ".")
          $(mkRelFile "test055.juvix")
          []
          $ checkTracesAndOutput
            [[nock| [[[[1 2] 3] [[2 3] 4] nil] [1 2] [2 3] nil] |]],
        mkAnomaTest
          56
          AnomaTestModeFull
          "Argument specialization"
          $(mkRelDir ".")
          $(mkRelFile "test056.juvix")
          []
          $ checkNatOutput [69],
        mkAnomaTest
          57
          AnomaTestModeFull
          "Case folding"
          $(mkRelDir ".")
          $(mkRelFile "test057.juvix")
          []
          $ checkNatOutput [8],
        mkAnomaTest
          58
          AnomaTestModeDebugOnly
          "Ranges"
          $(mkRelDir ".")
          $(mkRelFile "test058.juvix")
          []
          $ checkNatOutput [7550],
        mkAnomaTest
          59
          AnomaTestModeFull
          "Builtin list"
          $(mkRelDir ".")
          $(mkRelFile "test059.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          60
          AnomaTestModeFull
          "Record update"
          $(mkRelDir ".")
          $(mkRelFile "test060.juvix")
          []
          $ checkTracesAndOutput [[nock| [30 10 2] |]],
        mkAnomaTest
          61
          AnomaTestModeFull
          "Traits"
          $(mkRelDir ".")
          $(mkRelFile "test061.juvix")
          []
          $ checkNatOutput [1, 0, 3, 5, 1, 6, 5, 3, 1, 1, 6, 1, 3],
        mkAnomaTest
          62
          AnomaTestModeFull
          "Overapplication"
          $(mkRelDir ".")
          $(mkRelFile "test062.juvix")
          []
          $ checkNatOutput [1],
        mkAnomaTest
          63
          AnomaTestModeFull
          "Coercions"
          $(mkRelDir ".")
          $(mkRelFile "test063.juvix")
          []
          $ checkNatOutput [0, 1, 2, 300, 4, 5, 6, 7],
        mkAnomaTest
          64
          AnomaTestModeFull
          "Constant folding"
          $(mkRelDir ".")
          $(mkRelFile "test064.juvix")
          []
          $ checkNatOutput [37],
        mkAnomaTest
          65
          AnomaTestModeFull
          "Arithmetic simplification"
          $(mkRelDir ".")
          $(mkRelFile "test065.juvix")
          []
          $ checkNatOutput [42],
        mkAnomaTest
          66
          AnomaTestModeFull
          "Import function with a function call in default argument"
          $(mkRelDir "test066")
          $(mkRelFile "M.juvix")
          []
          $ checkNatOutput [0],
        mkAnomaTest
          67
          AnomaTestModeFull
          "Dependent default values inserted during translation FromConcrete"
          $(mkRelDir ".")
          $(mkRelFile "test067.juvix")
          []
          $ checkNatOutput [30],
        mkAnomaTest
          68
          AnomaTestModeFull
          "Dependent default values inserted in the arity checker"
          $(mkRelDir ".")
          $(mkRelFile "test068.juvix")
          []
          $ checkNatOutput [30],
        mkAnomaTest
          69
          AnomaTestModeFull
          "Dependent default values for Ord trait"
          $(mkRelDir ".")
          $(mkRelFile "test069.juvix")
          []
          $ checkTracesAndOutput [[nock| true |]],
        mkAnomaTest
          70
          AnomaTestModeFull
          "Nested default values and named arguments"
          $(mkRelDir ".")
          $(mkRelFile "test070.juvix")
          []
          $ checkNatOutput [1463],
        mkAnomaTest
          71
          AnomaTestModeFull
          "Named application (Ord instance with default cmp)"
          $(mkRelDir ".")
          $(mkRelFile "test071.juvix")
          []
          $ checkNatOutput [1528],
        mkAnomaTest
          72
          AnomaTestModeFull
          "Monad transformers (ReaderT + StateT + Identity)"
          $(mkRelDir "test072")
          $(mkRelFile "ReaderT.juvix")
          []
          $ checkNatOutput [10],
        mkAnomaTest
          73
          AnomaTestModeFull
          "Import and use a syntax alias"
          $(mkRelDir "test073")
          $(mkRelFile "test073.juvix")
          []
          $ checkNatOutput [11],
        let k1 :: Term Natural = [nock| 333 |]
            v1 :: Term Natural = [nock| 222 |]
            k2 :: Term Natural = [nock| [1 2 3 nil] |]
            v2 :: Term Natural = [nock| [4 5 6 nil] |]
         in mkAnomaTest'
              AnomaTestModeDebugOnly
              ( Storage
                  ( hashMap
                      [ (StorageKey k1, v1),
                        (StorageKey k2, v2)
                      ]
                  )
              )
              74
              "Builtin anomaGet"
              $(mkRelDir ".")
              $(mkRelFile "test074.juvix")
              [k1, k2]
              $ checkTracesAndOutput [v1, v2],
        mkAnomaTest
          75
          AnomaTestModeDebugOnly
          "Anoma encode"
          $(mkRelDir ".")
          $(mkRelFile "test075.juvix")
          []
          $ checkNatOutput [2, 84081, 4657, 12],
        mkAnomaTest
          76
          AnomaTestModeDebugOnly
          "Anoma decode"
          $(mkRelDir ".")
          $(mkRelFile "test076.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 0 |],
              [nock| [1 2 0] |],
              [nock| [1 2] |],
              [nock| false |]
            ],
        mkAnomaTest
          77
          AnomaTestModeDebugOnly
          "Anoma verify-detached"
          $(mkRelDir ".")
          $(mkRelFile "test077.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 64 |],
              [nock| true |]
            ],
        let toSignAndVerify :: Term Natural = [nock| [1 2 nil] |]
         in mkAnomaTest
              78
              AnomaTestModeDebugOnly
              "Anoma sign and verify"
              $(mkRelDir ".")
              $(mkRelFile "test078.juvix")
              [toSignAndVerify]
              $ checkTracesAndOutput
                [toSignAndVerify],
        let inputStr :: Term Natural = [nock| "Juvix!" |]
         in mkAnomaTest
              79
              AnomaTestModeDebugOnly
              "Strings"
              $(mkRelDir ".")
              $(mkRelFile "test079.juvix")
              [inputStr]
              $ checkTracesAndOutput [[nock| "Juvix! ✨ héllo world ✨" |]],
        mkAnomaTest
          80
          AnomaTestModeFull
          "Maybe"
          $(mkRelDir ".")
          $(mkRelFile "test080.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| [nil 1] |],
              [nock| 2 |],
              [nock| 3 |],
              [nock| nil |]
            ],
        mkAnomaTest
          81
          AnomaTestModeDebugOnly
          "UInt8"
          $(mkRelDir ".")
          $(mkRelFile "test081.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 1 |],
              [nock| 255 |],
              [nock| 2 |],
              [nock| true |],
              [nock| true |],
              [nock| false |],
              [nock| 1 |],
              [nock| 238 |],
              [nock| 3 |],
              [nock| 240 |],
              [nock| [1 238 3 2 nil] |]
            ],
        mkAnomaTest
          82
          AnomaTestModeFull
          "ByteArray"
          $(mkRelDir ".")
          $(mkRelFile "test082.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 0 |],
              [nock| [0 0] |],
              [nock| 3 |],
              [nock| [3 0] |],
              [nock| 4 |],
              [nock| [4 1] |],
              [nock| 2 |],
              [nock| [2 258] |],
              [nock| 1 |],
              [nock| [1 0] |]
            ],
        mkAnomaTest
          83
          AnomaTestModeFull
          "Anoma ByteArray"
          $(mkRelDir ".")
          $(mkRelFile "test083.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| [[0 0] 0] |],
              [nock| [[3 0] 0] |],
              [nock| [[4 1] 1] |],
              [nock| [[2 258] 258] |],
              [nock| [[1 0] 0] |]
            ],
        mkAnomaTest
          84
          AnomaTestModeDebugOnly
          "Anoma Sha256"
          $(mkRelDir ".")
          $(mkRelFile "test084.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| 32 |],
              [nock|
                  [
                    32
                    69779012276202546540741613998220636891790827476075440677599814057037833368907
                  ] |]
            ],
        mkAnomaTest
          85
          AnomaTestModeNodeOnly
          "Anoma Resource Machine builtins"
          $(mkRelDir "test085")
          $(mkRelFile "delta.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| true |],
              [nock| true |],
              [nock| false |],
              [nock| false |],
              [nock| 2 |],
              [nock| 2 |],
              [nock| 2 |],
              [nock| 2 |],
              [nock| true |]
            ],
        mkAnomaTest
          86
          AnomaTestModeDebugOnly
          "Anoma Random"
          $(mkRelDir ".")
          $(mkRelFile "test086.juvix")
          []
          $ checkTracesAndOutput
            [ [nock| [2 30764] |],
              [nock| [3 10689019] |],
              [nock| [2 20159] |],
              [nock| [4 4187579825] |]
            ],
        let testList :: Term Natural = [nock| [1 2 nil] |]
            expectedOutput :: Term Natural = [nock| [2 1 nil] |]
         in mkAnomaTest
              87
              AnomaTestModeNodeOnly
              "AnomaSet"
              $(mkRelDir ".")
              $(mkRelFile "test087.juvix")
              [testList]
              $ checkTracesAndOutput [expectedOutput],
        mkAnomaTest
          88
          AnomaTestModeFull
          "Noun type representation"
          $(mkRelDir ".")
          $(mkRelFile "test088.juvix")
          []
          $ checkTracesAndOutput [[nock| [30 40 80] |], [nock| [30 40 80] |], [nock| 80 |], [nock| [0 0] |]],
        mkAnomaTest
          89
          AnomaTestModeNodeOnly
          "Keccak256"
          $(mkRelDir ".")
          $(mkRelFile "test089.juvix")
          []
          $ checkTracesAndOutput
            [ [nock|
                  [
                    32
                   90876768632225629395702952947791536168367734137399490612004543592448844073500
                  ] |]
            ],
        mkAnomaTest
          90
          AnomaTestModeNodeOnly
          "Secp256k1"
          $(mkRelDir ".")
          $(mkRelFile "test090.juvix")
          []
          $ checkTracesAndOutput
            [ [nock|
                   494314595042081500440857251381770292616938994877034494695646244172503432267828341988295847615050091656182697698441111518765263317712889741906029933485191684
                   |],
              [nock|
                   494314595042081500440857251381770292616938994877034494695646244172503432267828341988295847615050091656182697698441111518765263317712889741906029933485191684
                   |],
              [nock|
                   0
                   |],
              [nock|
                   1
                   |]
            ]
      ]
