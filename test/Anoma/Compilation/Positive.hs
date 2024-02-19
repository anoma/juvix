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

mkAnomaCallTest :: Text -> Prelude.Path Rel Dir -> Prelude.Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest _testName relRoot mainFile args _testCheck =
  testCase (unpack _testName) (mkTestIO >>= mkNockmaAssertion)
  where
    rootDir' :: Prelude.Path Abs Dir
    rootDir' = root <//> relRoot

    mkTestIO :: IO Test
    mkTestIO = do
      entryPoint <- set entryPointTarget TargetAnoma <$> testDefaultEntryPointIO rootDir' (rootDir' <//> mainFile)
      _testProgramSubject <- (^. pipelineResult) . snd <$> testRunIO entryPoint upToAnoma
      let _testProgramFormula = anomaCall args
          _testEvalOptions = defaultEvalOptions
      return Test {..}

allTests :: TestTree
allTests =
  testGroup
    "Anoma positive tests"
    [ mkAnomaCallTest
        "Branching"
        $(mkRelDir ".")
        $(mkRelFile "test001.juvix")
        [nockNatLiteral 3]
        (eqNock [nock| false |])
    ]
