module Anoma.Compilation.Positive where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude qualified as Prelude
import Nockma.Eval.Positive

root :: Prelude.Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Compilation/positive")

mkAnomaCallTest' :: Bool -> Storage Natural -> Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest' enableDebug _testProgramStorage _testName relRoot mainFile args _testCheck =
  testCase (unpack _testName) (mkTestIO >>= mkNockmaAssertion)
  where
    mkTestIO :: IO Test
    mkTestIO = do
      anomaRes <- withRootCopy compileMain
      let _testProgramFormula = anomaCall args
          _testProgramSubject = anomaRes ^. anomaClosure
          _testEvalOptions = defaultEvalOptions
          _testAssertEvalError :: Maybe (NockEvalError Natural -> Assertion) = Nothing
      return Test {..}

    withRootCopy :: (Prelude.Path Abs Dir -> IO a) -> IO a
    withRootCopy action = withSystemTempDir "test" $ \tmpRootDir -> do
      copyDirRecur root tmpRootDir
      action tmpRootDir

    compileMain :: Prelude.Path Abs Dir -> IO AnomaResult
    compileMain rootCopyDir = do
      let testRootDir = rootCopyDir <//> relRoot
      entryPoint <-
        set entryPointTarget (Just TargetAnoma) . set entryPointDebug enableDebug
          <$> testDefaultEntryPointIO testRootDir (testRootDir <//> mainFile)
      (^. pipelineResult) . snd <$> testRunIO entryPoint upToAnoma

mkAnomaCallTestNoTrace :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTestNoTrace = mkAnomaCallTest' False emptyStorage

mkAnomaCallTest :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest = mkAnomaCallTest' True emptyStorage

checkNatOutput :: [Natural] -> Check ()
checkNatOutput = checkOutput . fmap toNock

checkOutput :: [Term Natural] -> Check ()
checkOutput expected = case unsnoc expected of
  Nothing -> eqTraces [] >> eqNock (TermAtom nockVoid)
  Just (xs, x) -> do
    eqTraces xs
    eqNock x

allTests :: TestTree
allTests =
  testGroup
    "Anoma positive tests"
    [ mkAnomaCallTest
        "Test001: Arithmetic operators"
        $(mkRelDir ".")
        $(mkRelFile "test001.juvix")
        [nockNatLiteral 5]
        (checkNatOutput [11]),
      mkAnomaCallTest
        "Test002: Arithmetic operators inside lambdas"
        $(mkRelDir ".")
        $(mkRelFile "test002.juvix")
        [nockNatLiteral 2]
        (checkNatOutput [11]),
      mkAnomaCallTest
        "Test003: Integer arithmetic"
        $(mkRelDir ".")
        $(mkRelFile "test003.juvix")
        []
        (checkNatOutput [1, 4, 2, 4, 0]),
      mkAnomaCallTestNoTrace
        "Test003: Integer arithmetic - no trace"
        $(mkRelDir ".")
        $(mkRelFile "test003.juvix")
        []
        (checkNatOutput [0]),
      mkAnomaCallTest
        "Test005: Higher-order functions"
        $(mkRelDir ".")
        $(mkRelFile "test005.juvix")
        [nockNatLiteral 1]
        (checkNatOutput [6]),
      mkAnomaCallTest
        "Test006: If-then-else and lazy boolean operators"
        $(mkRelDir ".")
        $(mkRelFile "test006.juvix")
        []
        (checkOutput [[nock| 2 |], [nock| true |], [nock| false |]]),
      mkAnomaCallTest
        "Test007: Pattern matching and lambda-case"
        $(mkRelDir ".")
        $(mkRelFile "test007.juvix")
        []
        $ do
          let l :: Term Natural = [nock| [1 2 nil] |]
          checkOutput [[nock| false |], [nock| true |], [nock| 0 |], [nock| [1 nil] |], [nock| 1 |], l, l],
      mkAnomaCallTest
        "Test008: Recursion"
        $(mkRelDir ".")
        $(mkRelFile "test008.juvix")
        [nockNatLiteral 1000]
        (eqNock [nock| 500500 |]),
      mkAnomaCallTest
        "Test009: Tail recursion"
        $(mkRelDir ".")
        $(mkRelFile "test009.juvix")
        [nockNatLiteral 1000]
        $ checkNatOutput [500500, 120, 3628800, 479001600],
      mkAnomaCallTest
        "Test010: Let"
        $(mkRelDir ".")
        $(mkRelFile "test010.juvix")
        []
        (checkNatOutput [32]),
      mkAnomaCallTest
        "Test011: Tail recursion: Fibonacci numbers in linear time"
        $(mkRelDir ".")
        $(mkRelFile "test011.juvix")
        []
        $ do
          let fib10 :: Natural = 55
              fib100 :: Natural = 354224848179261915075
              fib1000 :: Natural = 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
          checkNatOutput [fib10, fib100, fib1000],
      mkAnomaCallTest
        "Test012: Trees"
        $(mkRelDir ".")
        $(mkRelFile "test012.juvix")
        [nockNatLiteral 1000]
        $ checkNatOutput
          [ 13200200200,
            21320020020013200200200,
            3213200200200132002002002132002002001320020020021320020020013200200200,
            13213200200200132002002002132002002001320020020021320020020013200200200,
            21321320020020013200200200213200200200132002002002132002002001320020020013213200200200132002002002132002002001320020020021320020020013200200200
          ],
      mkAnomaCallTest
        "Test013: Functions returning functions with variable capture"
        $(mkRelDir ".")
        $(mkRelFile "test013.juvix")
        []
        $ checkNatOutput [1, 0, 2, 5],
      mkAnomaCallTest
        "Test014: Arithmetic"
        $(mkRelDir ".")
        $(mkRelFile "test014.juvix")
        []
        $ checkNatOutput [7, 17, 37, 31],
      mkAnomaCallTest
        "Test015: Local functions with free variables"
        $(mkRelDir ".")
        $(mkRelFile "test015.juvix")
        []
        $ checkNatOutput [600, 25, 30, 45, 55, 16],
      mkAnomaCallTest
        "Test016: Recursion through higher-order functions"
        $(mkRelDir ".")
        $(mkRelFile "test016.juvix")
        []
        $ checkNatOutput [55],
      mkAnomaCallTest
        "Test017: Tail recursion through higher-order functions"
        $(mkRelDir ".")
        $(mkRelFile "test017.juvix")
        [nockNatLiteral 1000]
        $ checkNatOutput [500500],
      mkAnomaCallTest
        "Test018: Higher-order functions and recursion"
        $(mkRelDir ".")
        $(mkRelFile "test018.juvix")
        []
        $ checkNatOutput [11],
      mkAnomaCallTest
        "Test019: Self-application"
        $(mkRelDir ".")
        $(mkRelFile "test019.juvix")
        []
        $ checkNatOutput [7],
      mkAnomaCallTest
        "Test020: Recursive functions: McCarthy's 91 function, subtraction by increments"
        $(mkRelDir ".")
        $(mkRelFile "test020.juvix")
        []
        $ checkNatOutput [91, 91, 91, 91, 100, 6, 6, 400, 4000],
      mkAnomaCallTest
        "Test021: Fast exponentiation"
        $(mkRelDir ".")
        $(mkRelFile "test021.juvix")
        []
        $ checkNatOutput [8, 2187, 48828125],
      mkAnomaCallTest
        "Test022: Lists"
        $(mkRelDir ".")
        $(mkRelFile "test022.juvix")
        [nockNatLiteral 1000]
        $ checkOutput
          [ [nock| [10 9 8 7 6 5 4 3 2 1 nil] |],
            [nock| [1 2 3 4 5 6 7 8 9 10 nil] |],
            [nock| [10 9 8 7 6 nil] |],
            [nock| [0 1 2 3 4 5 6 7 8 9 nil] |],
            [nock| 500500 |],
            [nock| 500500 |]
          ],
      mkAnomaCallTest
        "Test023: Mutual recursion"
        $(mkRelDir ".")
        $(mkRelFile "test023.juvix")
        []
        $ checkNatOutput [32, 869, 6385109],
      mkAnomaCallTest
        "Test024: Nested binders with variable capture"
        $(mkRelDir ".")
        $(mkRelFile "test024.juvix")
        []
        $ checkNatOutput [6688],
      mkAnomaCallTest
        "Test025: Euclid's algorithm"
        $(mkRelDir ".")
        $(mkRelFile "test025.juvix")
        []
        $ checkNatOutput [14, 70, 1, 1, 1],
      mkAnomaCallTest
        "Test026: Functional queues"
        $(mkRelDir ".")
        $(mkRelFile "test026.juvix")
        []
        $ checkOutput [makeList (toNock @Natural <$> [1 .. 100])],
      -- TODO allow lambda branches of different number of patterns
      -- mkAnomaCallTest
      --   "Test027: Church numerals"
      --   $(mkRelDir ".")
      --   $(mkRelFile "test027.juvix")
      --   []
      --   $ checkNatOutput [7, 10, 21],
      mkAnomaCallTest
        "Test028: Streams without memoization"
        $(mkRelDir ".")
        $(mkRelFile "test028.juvix")
        [nockNatLiteral 10, nockNatLiteral 50]
        $ checkNatOutput [31, 233],
      mkAnomaCallTest
        "Test029: Ackermann function"
        $(mkRelDir ".")
        $(mkRelFile "test029.juvix")
        []
        $ checkNatOutput [8, 9, 15, 17, 29],
      mkAnomaCallTest
        "Test030: Ackermann function (higher-order definition)"
        $(mkRelDir ".")
        $(mkRelFile "test030.juvix")
        []
        $ checkNatOutput [10, 21, 2187, 15],
      mkAnomaCallTest
        "Test031: Nested lists"
        $(mkRelDir ".")
        $(mkRelFile "test031.juvix")
        []
        $ checkOutput [[nock| [4 3 2 1 3 2 1 2 1 1 nil ] |]],
      mkAnomaCallTest
        "Test032: Merge sort"
        $(mkRelDir ".")
        $(mkRelFile "test032.juvix")
        []
        $ do
          let l = makeList (toNock @Natural <$> [2 .. 11])
          checkOutput [l, l, l],
      mkAnomaCallTest
        "Test033: Eta-expansion of builtins and constructors"
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
      mkAnomaCallTest
        "Test034: Recursive let"
        $(mkRelDir ".")
        $(mkRelFile "test034.juvix")
        []
        $ checkNatOutput [500500, 32, 869, 41, 85],
      mkAnomaCallTest
        "Test035: Pattern matching"
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
      mkAnomaCallTest
        "Test036: Eta-expansion"
        $(mkRelDir ".")
        $(mkRelFile "test036.juvix")
        []
        $ checkNatOutput [18],
      mkAnomaCallTest
        "Test037: Applications with lets and cases in function position"
        $(mkRelDir ".")
        $(mkRelFile "test037.juvix")
        []
        $ checkNatOutput [9],
      mkAnomaCallTest
        "Test038: Simple case expression"
        $(mkRelDir ".")
        $(mkRelFile "test038.juvix")
        []
        $ checkNatOutput [1],
      mkAnomaCallTest
        "Test039: Mutually recursive let expression"
        $(mkRelDir ".")
        $(mkRelFile "test039.juvix")
        []
        $ checkOutput [[nock| false |], [nock| true |]],
      mkAnomaCallTest
        "Test040: Pattern matching nullary constructor"
        $(mkRelDir ".")
        $(mkRelFile "test040.juvix")
        []
        $ checkOutput [[nock| true |]],
      mkAnomaCallTest
        "Test041: Use a builtin inductive in an inductive constructor"
        $(mkRelDir ".")
        $(mkRelFile "test041.juvix")
        []
        $ checkNatOutput [6],
      mkAnomaCallTest
        "Test043: Builtin trace"
        $(mkRelDir ".")
        $(mkRelFile "test043.juvix")
        []
        $ checkNatOutput [0, 1],
      mkAnomaCallTest
        "Test046: Polymorphic type arguments"
        $(mkRelDir ".")
        $(mkRelFile "test046.juvix")
        []
        $ checkNatOutput [7],
      mkAnomaCallTest
        "Test047: Local Modules"
        $(mkRelDir ".")
        $(mkRelFile "test047.juvix")
        []
        $ checkNatOutput [660],
      mkAnomaCallTest
        "Test049: Builtin Int"
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
      mkAnomaCallTest
        "Test050: Pattern matching with integers"
        $(mkRelDir ".")
        $(mkRelFile "test050.juvix")
        []
        $ checkNatOutput [11],
      mkAnomaCallTest
        "Test052: Simple lambda calculus"
        $(mkRelDir ".")
        $(mkRelFile "test052.juvix")
        []
        $ checkOutput [[nock| [15 nil] |]],
      mkAnomaCallTest
        "Test053: Inlining"
        $(mkRelDir ".")
        $(mkRelFile "test053.juvix")
        []
        $ checkNatOutput [21],
      mkAnomaCallTest
        "Test054: Iterators"
        $(mkRelDir ".")
        $(mkRelFile "test054.juvix")
        []
        $ checkNatOutput [189],
      mkAnomaCallTest
        "Test055: Constructor printing"
        $(mkRelDir ".")
        $(mkRelFile "test055.juvix")
        []
        $ checkOutput
          [[nock| [[[[1 2] 3] [[2 3] 4] nil] [1 2] [2 3] nil] |]],
      mkAnomaCallTest
        "Test056: Argument specialization"
        $(mkRelDir ".")
        $(mkRelFile "test056.juvix")
        []
        $ checkNatOutput [69],
      mkAnomaCallTest
        "Test057: Case folding"
        $(mkRelDir ".")
        $(mkRelFile "test057.juvix")
        []
        $ checkNatOutput [8],
      mkAnomaCallTest
        "Test058: Ranges"
        $(mkRelDir ".")
        $(mkRelFile "test058.juvix")
        []
        $ checkNatOutput [7550],
      mkAnomaCallTest
        "Test059: Builtin list"
        $(mkRelDir ".")
        $(mkRelFile "test059.juvix")
        []
        $ checkNatOutput [11],
      mkAnomaCallTest
        "Test060: Record update"
        $(mkRelDir ".")
        $(mkRelFile "test060.juvix")
        []
        $ checkOutput [[nock| [30 10 2] |]],
      mkAnomaCallTest
        "Test061: Traits"
        $(mkRelDir ".")
        $(mkRelFile "test061.juvix")
        []
        $ checkNatOutput [1, 0, 3, 5, 1, 6, 5, 3, 1, 1, 6, 1, 3],
      mkAnomaCallTest
        "Test062: Overapplication"
        $(mkRelDir ".")
        $(mkRelFile "test062.juvix")
        []
        $ checkNatOutput [1],
      mkAnomaCallTest
        "Test063: Coercions"
        $(mkRelDir ".")
        $(mkRelFile "test063.juvix")
        []
        $ checkNatOutput [0, 1, 2, 300, 4, 5, 6, 7],
      mkAnomaCallTest
        "Test064: Constant folding"
        $(mkRelDir ".")
        $(mkRelFile "test064.juvix")
        []
        $ checkNatOutput [37],
      mkAnomaCallTest
        "Test065: Arithmetic simplification"
        $(mkRelDir ".")
        $(mkRelFile "test065.juvix")
        []
        $ checkNatOutput [42],
      mkAnomaCallTest
        "Test066: Import function with a function call in default argument"
        $(mkRelDir "test066")
        $(mkRelFile "M.juvix")
        []
        $ checkNatOutput [0],
      mkAnomaCallTest
        "Test067: Dependent default values inserted during translation FromConcrete"
        $(mkRelDir ".")
        $(mkRelFile "test067.juvix")
        []
        $ checkNatOutput [30],
      mkAnomaCallTest
        "Test068: Dependent default values inserted in the arity checker"
        $(mkRelDir ".")
        $(mkRelFile "test068.juvix")
        []
        $ checkNatOutput [30],
      mkAnomaCallTest
        "Test069: Dependent default values for Ord trait"
        $(mkRelDir ".")
        $(mkRelFile "test069.juvix")
        []
        $ checkOutput [[nock| true |]],
      mkAnomaCallTest
        "Test070: Nested default values and named arguments"
        $(mkRelDir ".")
        $(mkRelFile "test070.juvix")
        []
        $ checkNatOutput [1463],
      mkAnomaCallTest
        "Test071: Named application (Ord instance with default cmp)"
        $(mkRelDir ".")
        $(mkRelFile "test071.juvix")
        []
        $ checkNatOutput [1528],
      mkAnomaCallTest
        "Test072: Monad transformers (ReaderT + StateT + Identity)"
        $(mkRelDir "test072")
        $(mkRelFile "ReaderT.juvix")
        []
        $ checkNatOutput [10],
      mkAnomaCallTest
        "Test073: Import and use a syntax alias"
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
       in mkAnomaCallTest'
            True
            ( Storage
                ( HashMap.fromList
                    [ (StorageKey sk1, v1),
                      (StorageKey sk2, v2)
                    ]
                )
            )
            "Test074: Builtin anomaGet"
            $(mkRelDir ".")
            $(mkRelFile "test074.juvix")
            [OpQuote # k1, OpQuote # k2]
            $ checkOutput [v1, v2],
      mkAnomaCallTest
        "Test075: Anoma encode"
        $(mkRelDir ".")
        $(mkRelFile "test075.juvix")
        []
        $ checkNatOutput [2, 84081, 4657, 12],
      mkAnomaCallTest
        "Test076: Anoma decode"
        $(mkRelDir ".")
        $(mkRelFile "test076.juvix")
        []
        $ checkOutput
          [ [nock| 0 |],
            [nock| [1 2 0] |],
            [nock| [1 2] |],
            [nock| false |]
          ],
      mkAnomaCallTest
        "Test077: Anoma verify-detached"
        $(mkRelDir ".")
        $(mkRelFile "test077.juvix")
        []
        $ checkOutput
          [ [nock| true |]
          ],
      let toSignAndVerify :: Term Natural = [nock| [1 2 nil] |]
       in mkAnomaCallTest
            "Test078: Anoma sign and verify"
            $(mkRelDir ".")
            $(mkRelFile "test078.juvix")
            [OpQuote # toSignAndVerify]
            $ checkOutput
              [toSignAndVerify],
      let inputStr :: Term Natural = [nock| "Juvix!" |]
       in mkAnomaCallTest
            "Test079: Strings"
            $(mkRelDir ".")
            $(mkRelFile "test079.juvix")
            [OpQuote # inputStr]
            $ checkOutput [[nock| "Juvix! ✨ héllo world ✨" |]]
    ]
