module Anoma.Compilation.Positive (allTests) where

import Anoma.Effect.Base
import Anoma.Effect.RunNockma
import Base
import Data.IntSet qualified as IntSet
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude qualified as Prelude
import Nockma.Eval.Positive (Check, Test (..), eqNock, eqTraces)
import Nockma.Eval.Positive qualified as NockmaEval

data AnomaTest = AnomaTest
  { _anomaEnableDebug :: Bool,
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

fromAnomaTest :: AnomaTest -> TestTree
fromAnomaTest a@AnomaTest {..} =
  testCase testname (mkTestIO >>= NockmaEval.mkNockmaAssertion)
  where
    testname :: Text
    testname = anomaTestName a

    mkTestIO :: IO Test
    mkTestIO = do
      anomaRes <- withRootCopy (compileMain _anomaEnableDebug _anomaRelRoot _anomaMainFile)
      let _testProgramFormula = anomaCall (map (opQuote "Quote arg") _anomaArgs)
          _testProgramSubject = anomaRes ^. anomaClosure
          _testEvalOptions = defaultEvalOptions
          _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion) = Nothing
      return
        Test
          { _testName = testname,
            _testCheck = _anomaCheck,
            _testProgramStorage = _anomaProgramStorage,
            ..
          }

mkAnomaTest' ::
  Bool ->
  Storage Natural ->
  Int ->
  Text ->
  Prelude.Path Rel Dir ->
  Prelude.Path Rel File ->
  [Term Natural] ->
  Check () ->
  AnomaTest
mkAnomaTest' _anomaEnableDebug _anomaProgramStorage _anomaTestNum _anomaTestTag _anomaRelRoot _anomaMainFile _anomaArgs _anomaCheck =
  AnomaTest
    { ..
    }

envAnomaPath :: (MonadIO m) => m AnomaPath
envAnomaPath = AnomaPath <$> getAnomaPathAbs

mkAnomaNodeTest :: AnomaTest -> TestTree
mkAnomaNodeTest a@AnomaTest {..} =
  testCase (anomaTestName a) assertion
  where
    assertion :: Assertion
    assertion = do
      program :: Term Natural <- (^. anomaClosure) <$> withRootCopy (compileMain False _anomaRelRoot _anomaMainFile)
      -- For some reason the evaluation fails if no args are given
      let args'
            | null _anomaArgs = [toNock (nockVoid @Natural)]
            | otherwise = _anomaArgs
      testAnomaPath <- envAnomaPath
      runM
        . ignoreLogger
        . runSimpleErrorHUnit
        . runAnoma testAnomaPath
        $ do
          out <- runNockma program args'
          runM
            . runReader out
            . runReader []
            $ _anomaCheck

withRootCopy :: (Prelude.Path Abs Dir -> IO a) -> IO a
withRootCopy action = withSystemTempDir "test" $ \tmpRootDir -> do
  copyDirRecur root tmpRootDir
  action tmpRootDir

compileMain :: Bool -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> Prelude.Path Abs Dir -> IO AnomaResult
compileMain enableDebug relRoot mainFile rootCopyDir = do
  let testRootDir = rootCopyDir <//> relRoot
  entryPoint <-
    set entryPointTarget (Just TargetAnoma) . set entryPointDebug enableDebug
      <$> testDefaultEntryPointIO testRootDir (testRootDir <//> mainFile)
  (^. pipelineResult) . snd <$> testRunIO entryPoint upToAnoma

mkAnomaTestNoDebug ::
  Int ->
  Text ->
  Prelude.Path Rel Dir ->
  Prelude.Path Rel File ->
  [Term Natural] ->
  Check () ->
  AnomaTest
mkAnomaTestNoDebug = mkAnomaTest' False emptyStorage

mkAnomaTest ::
  Int ->
  Text ->
  Prelude.Path Rel Dir ->
  Prelude.Path Rel File ->
  [Term Natural] ->
  Check () ->
  AnomaTest
mkAnomaTest = mkAnomaTest' True emptyStorage

checkNatOutput :: [Natural] -> Check ()
checkNatOutput = checkOutput . fmap toNock

checkOutput :: [Term Natural] -> Check ()
checkOutput expected = case unsnoc expected of
  Nothing -> eqTraces [] >> eqNock (TermAtom nockVoid)
  Just (xs, x) -> do
    eqTraces xs
    eqNock x

anomaNodeValid :: IntSet
anomaNodeValid =
  intSet
    [ 1,
      2,
      5,
      8,
      10,
      16,
      17,
      18,
      19,
      24,
      26,
      31,
      36,
      37,
      38,
      40,
      41,
      45,
      46,
      47,
      50,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      62,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72
    ]

anomaNodeBug :: IntSet
anomaNodeBug =
  intSet
    [ 24,
      41,
      52,
      54,
      56
    ]

allTests :: TestTree
allTests =
  testGroup
    "Anoma positive tests"
    [ -- haskellNockmaTests,
      anomaNodeTests
    ]
  where
    anomaNodeTests :: TestTree
    anomaNodeTests =
      testGroup
        "AnomaNode"
        (map mkAnomaNodeTest (filter shouldRun anomaTests))
      where
        shouldRun :: AnomaTest -> Bool
        shouldRun AnomaTest {..} =
          IntSet.member _anomaTestNum anomaNodeValid
            && IntSet.notMember _anomaTestNum anomaNodeBug

    haskellNockmaTests :: TestTree
    haskellNockmaTests =
      -- withResource initNode freeNode $ \_ ->
      testGroup
        "Anoma positive tests (Haskell evaluator)"
        (map fromAnomaTest anomaTests)
    -- where
    -- initNode :: IO AnomaProcesses
    -- initNode = runM
    --   . runSimpleErrorHUnit
    --   . ignoreLogger
    --   . runAnoma testAnomaPath
    --   $ do
    --     noHalt

    -- freeNode :: AnomaProcesses -> IO ()
    -- freeNode AnomaProcesses {..} = runM . runProcess $ do
    --   terminateProcess _anomaClientHandle
    --   terminateProcess _anomaNodeHandle

    natArg :: Natural -> Term Natural
    natArg = toNock

    anomaTests :: [AnomaTest]
    anomaTests =
      [ mkAnomaTest
          1
          "Arithmetic operators"
          $(mkRelDir ".")
          $(mkRelFile "test001.juvix")
          [natArg 5]
          (checkNatOutput [11]),
        mkAnomaTest
          2
          "Arithmetic operators inside lambdas"
          $(mkRelDir ".")
          $(mkRelFile "test002.juvix")
          [natArg 2]
          (checkNatOutput [11]),
        mkAnomaTest
          3
          "Integer arithmetic"
          $(mkRelDir ".")
          $(mkRelFile "test003.juvix")
          []
          (checkNatOutput [1, 4, 2, 4, 0]),
        mkAnomaTestNoDebug
          3
          "Integer arithmetic - no debug"
          $(mkRelDir ".")
          $(mkRelFile "test003.juvix")
          []
          (checkNatOutput [1, 4, 2, 4, 0]),
        mkAnomaTest
          5
          "Higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test005.juvix")
          [natArg 1]
          (checkNatOutput [6]),
        mkAnomaTest
          6
          "If-then-else and lazy boolean operators"
          $(mkRelDir ".")
          $(mkRelFile "test006.juvix")
          []
          (checkOutput [[nock| 2 |], [nock| true |], [nock| false |]]),
        mkAnomaTest
          7
          "Pattern matching and lambda-case"
          $(mkRelDir ".")
          $(mkRelFile "test007.juvix")
          []
          $ do
            let l :: Term Natural = [nock| [1 2 nil] |]
            checkOutput [[nock| false |], [nock| true |], [nock| 0 |], [nock| [1 nil] |], [nock| 1 |], l, l],
        mkAnomaTest
          8
          "Recursion"
          $(mkRelDir ".")
          $(mkRelFile "test008.juvix")
          [natArg 1000]
          (eqNock [nock| 500500 |]),
        mkAnomaTest
          9
          "Tail recursion"
          $(mkRelDir ".")
          $(mkRelFile "test009.juvix")
          [natArg 1000]
          $ checkNatOutput [500500, 120, 3628800, 479001600],
        mkAnomaTest
          10
          "Let"
          $(mkRelDir ".")
          $(mkRelFile "test010.juvix")
          []
          (checkNatOutput [32]),
        mkAnomaTest
          11
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
          "Functions returning functions with variable capture"
          $(mkRelDir ".")
          $(mkRelFile "test013.juvix")
          []
          $ checkNatOutput [1, 0, 2, 5],
        mkAnomaTest
          14
          "Arithmetic"
          $(mkRelDir ".")
          $(mkRelFile "test014.juvix")
          []
          $ checkNatOutput [7, 17, 37, 31],
        mkAnomaTest
          15
          "Local functions with free variables"
          $(mkRelDir ".")
          $(mkRelFile "test015.juvix")
          []
          $ checkNatOutput [600, 25, 30, 45, 55, 16],
        mkAnomaTest
          16
          "Recursion through higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test016.juvix")
          []
          $ checkNatOutput [55],
        mkAnomaTest
          17
          "Tail recursion through higher-order functions"
          $(mkRelDir ".")
          $(mkRelFile "test017.juvix")
          [natArg 1000]
          $ checkNatOutput [500500],
        mkAnomaTest
          18
          "Higher-order functions and recursion"
          $(mkRelDir ".")
          $(mkRelFile "test018.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          19
          "Self-application"
          $(mkRelDir ".")
          $(mkRelFile "test019.juvix")
          []
          $ checkNatOutput [7],
        mkAnomaTest
          20
          "Recursive functions: McCarthy's 91 function, subtraction by increments"
          $(mkRelDir ".")
          $(mkRelFile "test020.juvix")
          []
          $ checkNatOutput [91, 91, 91, 91, 100, 6, 6, 400, 4000],
        mkAnomaTest
          21
          "Fast exponentiation"
          $(mkRelDir ".")
          $(mkRelFile "test021.juvix")
          []
          $ checkNatOutput [8, 2187, 48828125],
        mkAnomaTest
          22
          "Lists"
          $(mkRelDir ".")
          $(mkRelFile "test022.juvix")
          [natArg 1000]
          $ checkOutput
            [ [nock| [10 9 8 7 6 5 4 3 2 1 nil] |],
              [nock| [1 2 3 4 5 6 7 8 9 10 nil] |],
              [nock| [10 9 8 7 6 nil] |],
              [nock| [0 1 2 3 4 5 6 7 8 9 nil] |],
              [nock| 500500 |],
              [nock| 500500 |]
            ],
        mkAnomaTest
          23
          "Mutual recursion"
          $(mkRelDir ".")
          $(mkRelFile "test023.juvix")
          []
          $ checkNatOutput [32, 869, 6385109],
        mkAnomaTest
          24
          "Nested binders with variable capture"
          $(mkRelDir ".")
          $(mkRelFile "test024.juvix")
          []
          $ checkNatOutput [6688],
        mkAnomaTest
          25
          "Euclid's algorithm"
          $(mkRelDir ".")
          $(mkRelFile "test025.juvix")
          []
          $ checkNatOutput [14, 70, 1, 1, 1],
        mkAnomaTest
          26
          "Functional queues"
          $(mkRelDir ".")
          $(mkRelFile "test026.juvix")
          []
          $ checkOutput [makeList (toNock @Natural <$> [1 .. 100])],
        -- TODO allow lambda branches of different number of patterns
        -- mkAnomaTest
        --   "Test027: Church numerals"
        --   $(mkRelDir ".")
        --   $(mkRelFile "test027.juvix")
        --   []
        --   $ checkNatOutput [7, 10, 21],
        mkAnomaTest
          28
          "Streams without memoization"
          $(mkRelDir ".")
          $(mkRelFile "test028.juvix")
          [natArg 10, natArg 50]
          $ checkNatOutput [31, 233],
        mkAnomaTest
          29
          "Ackermann function"
          $(mkRelDir ".")
          $(mkRelFile "test029.juvix")
          []
          $ checkNatOutput [8, 9, 15, 17, 29],
        mkAnomaTest
          30
          "Ackermann function (higher-order definition)"
          $(mkRelDir ".")
          $(mkRelFile "test030.juvix")
          []
          $ checkNatOutput [10, 21, 2187, 15],
        mkAnomaTest
          31
          "Nested lists"
          $(mkRelDir ".")
          $(mkRelFile "test031.juvix")
          []
          $ checkOutput [[nock| [4 3 2 1 3 2 1 2 1 1 nil ] |]],
        mkAnomaTest
          32
          "Merge sort"
          $(mkRelDir ".")
          $(mkRelFile "test032.juvix")
          []
          $ do
            let l = makeList (toNock @Natural <$> [2 .. 11])
            checkOutput [l, l, l],
        mkAnomaTest
          33
          "Eta-expansion of builtins and constructors"
          $(mkRelDir ".")
          $(mkRelFile "test033.juvix")
          []
          $ checkOutput
            [ [nock| 9 |],
              [nock| [7 2] |],
              [nock| 5 |],
              [nock| [3 2] |],
              [nock| [1 2] |]
            ],
        mkAnomaTest
          34
          "Recursive let"
          $(mkRelDir ".")
          $(mkRelFile "test034.juvix")
          []
          $ checkNatOutput [500500, 32, 869, 41, 85],
        mkAnomaTest
          35
          "Pattern matching"
          $(mkRelDir ".")
          $(mkRelFile "test035.juvix")
          []
          $ checkOutput
            [ [nock| [9 7 5 3 1 nil] |],
              [nock| 300 |],
              [nock| 4160 |],
              [nock| 2336 |],
              [nock| 1 |],
              [nock| 0 |]
            ],
        mkAnomaTest
          36
          "Eta-expansion"
          $(mkRelDir ".")
          $(mkRelFile "test036.juvix")
          []
          $ checkNatOutput [18],
        mkAnomaTest
          37
          "Applications with lets and cases in function position"
          $(mkRelDir ".")
          $(mkRelFile "test037.juvix")
          []
          $ checkNatOutput [9],
        mkAnomaTest
          38
          "Simple case expression"
          $(mkRelDir ".")
          $(mkRelFile "test038.juvix")
          []
          $ checkNatOutput [1],
        mkAnomaTest
          39
          "Mutually recursive let expression"
          $(mkRelDir ".")
          $(mkRelFile "test039.juvix")
          []
          $ checkOutput [[nock| false |], [nock| true |]],
        mkAnomaTest
          40
          "Pattern matching nullary constructor"
          $(mkRelDir ".")
          $(mkRelFile "test040.juvix")
          []
          $ checkOutput [[nock| true |]],
        mkAnomaTest
          41
          "Use a builtin inductive in an inductive constructor"
          $(mkRelDir ".")
          $(mkRelFile "test041.juvix")
          []
          $ checkNatOutput [6],
        mkAnomaTest
          43
          "Builtin trace"
          $(mkRelDir ".")
          $(mkRelFile "test043.juvix")
          []
          $ checkNatOutput [0, 1],
        mkAnomaTest
          45
          "Implicit builtin bool"
          $(mkRelDir ".")
          $(mkRelFile "test045.juvix")
          []
          $ checkNatOutput [4],
        mkAnomaTest
          46
          "Polymorphic type arguments"
          $(mkRelDir ".")
          $(mkRelFile "test046.juvix")
          []
          $ checkNatOutput [7],
        mkAnomaTest
          47
          "Local Modules"
          $(mkRelDir ".")
          $(mkRelFile "test047.juvix")
          []
          $ checkNatOutput [660],
        mkAnomaTest
          49
          "Builtin Int"
          $(mkRelDir ".")
          $(mkRelFile "test049.juvix")
          []
          $ checkOutput
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
          "Pattern matching with integers"
          $(mkRelDir ".")
          $(mkRelFile "test050.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          52
          "Simple lambda calculus"
          $(mkRelDir ".")
          $(mkRelFile "test052.juvix")
          []
          $ checkOutput [[nock| [15 nil] |]],
        mkAnomaTest
          53
          "Inlining"
          $(mkRelDir ".")
          $(mkRelFile "test053.juvix")
          []
          $ checkNatOutput [21],
        mkAnomaTest
          54
          "Iterators"
          $(mkRelDir ".")
          $(mkRelFile "test054.juvix")
          []
          $ checkNatOutput [189],
        mkAnomaTest
          55
          "Constructor printing"
          $(mkRelDir ".")
          $(mkRelFile "test055.juvix")
          []
          $ checkOutput
            [[nock| [[[[1 2] 3] [[2 3] 4] nil] [1 2] [2 3] nil] |]],
        mkAnomaTest
          56
          "Argument specialization"
          $(mkRelDir ".")
          $(mkRelFile "test056.juvix")
          []
          $ checkNatOutput [69],
        mkAnomaTest
          57
          "Case folding"
          $(mkRelDir ".")
          $(mkRelFile "test057.juvix")
          []
          $ checkNatOutput [8],
        mkAnomaTest
          58
          "Ranges"
          $(mkRelDir ".")
          $(mkRelFile "test058.juvix")
          []
          $ checkNatOutput [7550],
        mkAnomaTest
          59
          "Builtin list"
          $(mkRelDir ".")
          $(mkRelFile "test059.juvix")
          []
          $ checkNatOutput [11],
        mkAnomaTest
          60
          "Record update"
          $(mkRelDir ".")
          $(mkRelFile "test060.juvix")
          []
          $ checkOutput [[nock| [30 10 2] |]],
        mkAnomaTest
          61
          "Traits"
          $(mkRelDir ".")
          $(mkRelFile "test061.juvix")
          []
          $ checkNatOutput [1, 0, 3, 5, 1, 6, 5, 3, 1, 1, 6, 1, 3],
        mkAnomaTest
          62
          "Overapplication"
          $(mkRelDir ".")
          $(mkRelFile "test062.juvix")
          []
          $ checkNatOutput [1],
        mkAnomaTest
          63
          "Coercions"
          $(mkRelDir ".")
          $(mkRelFile "test063.juvix")
          []
          $ checkNatOutput [0, 1, 2, 300, 4, 5, 6, 7],
        mkAnomaTest
          64
          "Constant folding"
          $(mkRelDir ".")
          $(mkRelFile "test064.juvix")
          []
          $ checkNatOutput [37],
        mkAnomaTest
          65
          "Arithmetic simplification"
          $(mkRelDir ".")
          $(mkRelFile "test065.juvix")
          []
          $ checkNatOutput [42],
        mkAnomaTest
          66
          "Import function with a function call in default argument"
          $(mkRelDir "test066")
          $(mkRelFile "M.juvix")
          []
          $ checkNatOutput [0],
        mkAnomaTest
          67
          "Dependent default values inserted during translation FromConcrete"
          $(mkRelDir ".")
          $(mkRelFile "test067.juvix")
          []
          $ checkNatOutput [30],
        mkAnomaTest
          68
          "Dependent default values inserted in the arity checker"
          $(mkRelDir ".")
          $(mkRelFile "test068.juvix")
          []
          $ checkNatOutput [30],
        mkAnomaTest
          69
          "Dependent default values for Ord trait"
          $(mkRelDir ".")
          $(mkRelFile "test069.juvix")
          []
          $ checkOutput [[nock| true |]],
        mkAnomaTest
          70
          "Nested default values and named arguments"
          $(mkRelDir ".")
          $(mkRelFile "test070.juvix")
          []
          $ checkNatOutput [1463],
        mkAnomaTest
          71
          "Named application (Ord instance with default cmp)"
          $(mkRelDir ".")
          $(mkRelFile "test071.juvix")
          []
          $ checkNatOutput [1528],
        mkAnomaTest
          72
          "Monad transformers (ReaderT + StateT + Identity)"
          $(mkRelDir "test072")
          $(mkRelFile "ReaderT.juvix")
          []
          $ checkNatOutput [10],
        mkAnomaTest
          73
          "Import and use a syntax alias"
          $(mkRelDir "test073")
          $(mkRelFile "test073.juvix")
          []
          $ checkNatOutput [11],
        let k1 :: Term Natural = [nock| 333 |]
            v1 :: Term Natural = [nock| 222 |]
            k2 :: Term Natural = [nock| [1 2 3 nil] |]
            v2 :: Term Natural = [nock| [4 5 6 nil] |]
            -- The keys of the storage are of the form [id key nil].
            -- The id is captured from the arguments tuple of the function.
            sk1 :: Term Natural = [nock| [[333 1 2 3 nil] 333 nil] |]
            sk2 :: Term Natural = [nock| [[333 1 2 3 nil] [1 2 3 nil] nil] |]
         in mkAnomaTest'
              True
              ( Storage
                  ( hashMap
                      [ (StorageKey sk1, v1),
                        (StorageKey sk2, v2)
                      ]
                  )
              )
              74
              "Builtin anomaGet"
              $(mkRelDir ".")
              $(mkRelFile "test074.juvix")
              [k1, k2]
              $ checkOutput [v1, v2],
        mkAnomaTest
          75
          "Anoma encode"
          $(mkRelDir ".")
          $(mkRelFile "test075.juvix")
          []
          $ checkNatOutput [2, 84081, 4657, 12],
        mkAnomaTest
          76
          "Anoma decode"
          $(mkRelDir ".")
          $(mkRelFile "test076.juvix")
          []
          $ checkOutput
            [ [nock| 0 |],
              [nock| [1 2 0] |],
              [nock| [1 2] |],
              [nock| false |]
            ],
        mkAnomaTest
          77
          "Anoma verify-detached"
          $(mkRelDir ".")
          $(mkRelFile "test077.juvix")
          []
          $ checkOutput
            [ [nock| 64 |],
              [nock| true |]
            ],
        let toSignAndVerify :: Term Natural = [nock| [1 2 nil] |]
         in mkAnomaTest
              78
              "Anoma sign and verify"
              $(mkRelDir ".")
              $(mkRelFile "test078.juvix")
              [toSignAndVerify]
              $ checkOutput
                [toSignAndVerify],
        let inputStr :: Term Natural = [nock| "Juvix!" |]
         in mkAnomaTest
              79
              "Strings"
              $(mkRelDir ".")
              $(mkRelFile "test079.juvix")
              [inputStr]
              $ checkOutput [[nock| "Juvix! ✨ héllo world ✨" |]],
        mkAnomaTest
          80
          "Maybe"
          $(mkRelDir ".")
          $(mkRelFile "test080.juvix")
          []
          $ checkOutput
            [ [nock| [nil 1] |],
              [nock| 2 |],
              [nock| 3 |],
              [nock| nil |]
            ],
        mkAnomaTest
          81
          "UInt8"
          $(mkRelDir ".")
          $(mkRelFile "test081.juvix")
          []
          $ checkOutput
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
          "ByteArray"
          $(mkRelDir ".")
          $(mkRelFile "test082.juvix")
          []
          $ checkOutput
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
          "Anoma ByteArray"
          $(mkRelDir ".")
          $(mkRelFile "test083.juvix")
          []
          $ checkOutput
            [ [nock| [[0 0] 0] |],
              [nock| [[3 0] 0] |],
              [nock| [[4 1] 1] |],
              [nock| [[2 258] 258] |],
              [nock| [[1 0] 0] |]
            ],
        mkAnomaTest
          84
          "Anoma Sha256"
          $(mkRelDir ".")
          $(mkRelFile "test084.juvix")
          []
          $ checkOutput
            [ [nock| 64 |],
              [nock|
              [
                64
                5092006196359674779938793937035252249221936503860319648757996882954518215195609232852607160812968472040491493412050369557521935588220586883008001462395444
              ] |]
            ],
        mkAnomaTest
          85
          "Anoma Resource Machine builtins"
          $(mkRelDir ".")
          $(mkRelFile "test085.juvix")
          []
          $ checkOutput
            [ [nock| [[[11 22] 110] 0] |],
              [nock| [10 11] |],
              [nock| 478793196187462788804451 |],
              [nock| 418565088612 |],
              [nock| 0 |]
            ],
        mkAnomaTest
          86
          "Anoma Random"
          $(mkRelDir ".")
          $(mkRelFile "test086.juvix")
          []
          $ checkOutput
            [ [nock| [2 30764] |],
              [nock| [3 10689019] |],
              [nock| [2 20159] |],
              [nock| [4 4187579825] |]
            ]
      ]
