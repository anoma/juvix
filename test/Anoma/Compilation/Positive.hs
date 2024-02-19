module Anoma.Compilation.Positive where

import Base
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude qualified as Prelude
import Nockma.Base
import Nockma.Eval.Positive

root :: Prelude.Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/Compilation/positive")

mkAnomaCallTest' :: Bool -> Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest' enableDebug _testName relRoot mainFile args _testCheck =
  testCase (unpack _testName) (mkTestIO >>= mkNockmaAssertion)
  where
    rootDir' :: Prelude.Path Abs Dir
    rootDir' = root <//> relRoot

    mkTestIO :: IO Test
    mkTestIO = do
      entryPoint <- set entryPointTarget TargetAnoma . set entryPointDebug enableDebug <$> testDefaultEntryPointIO rootDir' (rootDir' <//> mainFile)
      _testProgramSubject <- (^. pipelineResult) . snd <$> testRunIO entryPoint upToAnoma
      let _testProgramFormula = anomaCall args
          _testEvalOptions = defaultEvalOptions
      return Test {..}

mkAnomaCallTestNoTrace :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTestNoTrace = mkAnomaCallTest' False

mkAnomaCallTest :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest = mkAnomaCallTest' True

checkNatOutput :: [Natural] -> Check ()
checkNatOutput = checkOutput . fmap toNock

checkOutput :: [Term Natural] -> Check ()
checkOutput expected = case unsnoc expected of
  Nothing -> return ()
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
          checkOutput [[nock| false |], [nock| true |], [nock| 0 |], [nock| [1 nil] |], [nock| 1 |], l, l, l],
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
          ]
    ]
