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

allTests :: TestTree
allTests =
  testGroup
    "Anoma positive tests"
    [ mkAnomaCallTest
        "Test001: Arithmetic operators"
        $(mkRelDir ".")
        $(mkRelFile "test001.juvix")
        [nockNatLiteral 5]
        (eqNock [nock| 11 |]),
      mkAnomaCallTest
        "Test002: Arithmetic operators inside lambdas"
        $(mkRelDir ".")
        $(mkRelFile "test002.juvix")
        [nockNatLiteral 2]
        (eqNock [nock| 11 |]),
      mkAnomaCallTest
        "Test003: Integer arithmetic"
        $(mkRelDir ".")
        $(mkRelFile "test003.juvix")
        []
        (eqTraces [[nock| 1 |], [nock| 4 |], [nock| 2 |], [nock| 4 |]] >> eqNock [nock| 0 |]),
      mkAnomaCallTestNoTrace
        "Test003: Integer arithmetic - no trace"
        $(mkRelDir ".")
        $(mkRelFile "test003.juvix")
        []
        (eqTraces [] >> eqNock [nock| 0 |]),
      mkAnomaCallTest
        "Test005: Higher-order functions"
        $(mkRelDir ".")
        $(mkRelFile "test005.juvix")
        [nockNatLiteral 1]
        (eqNock [nock| 6 |]),
      mkAnomaCallTest
        "Test006: If-then-else and lazy boolean operators"
        $(mkRelDir ".")
        $(mkRelFile "test006.juvix")
        []
        (eqTraces [[nock| 2 |], [nock| true |]] >> eqNock [nock| false |]),
      mkAnomaCallTest
        "Test007: Pattern matching and lambda-case"
        $(mkRelDir ".")
        $(mkRelFile "test007.juvix")
        []
        $ do
          let l :: Term Natural = [nock| [1 2 nil] |]
          eqTraces [[nock| false |], [nock| true |], [nock| 0 |], [nock| [1 nil] |], [nock| 1 |], l, l]
          eqNock l,
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
        $ do
          eqTraces [[nock| 500500 |], [nock| 120 |], [nock| 3628800 |]]
          eqNock [nock| 479001600 |],
      mkAnomaCallTest
        "Test010: Let"
        $(mkRelDir ".")
        $(mkRelFile "test010.juvix")
        []
        (eqNock [nock| 32 |])
    ]
