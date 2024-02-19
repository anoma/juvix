module Anoma.Compilation.Positive where

import Base
import Juvix.Compiler.Backend (Target (TargetAnoma))
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromSource.QQ hiding (Path)
import Juvix.Compiler.Nockma.Translation.FromTree hiding (Path)
import Nockma.Base
import Nockma.Eval.Positive

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Anoma/positive/Compilation")

mkAnomaCallTest :: Text -> Path Rel Dir -> Path Rel File -> [Term Natural] -> Check () -> TestTree
mkAnomaCallTest _testName relRoot mainFile args _testCheck =
  testCase (unpack _testName) (mkTestIO >>= mkNockmaAssertion)
  where
    rootDir' :: Path Abs Dir
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
