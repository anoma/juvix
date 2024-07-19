module Isabelle.Positive where

import Base
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Pretty

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive/Isabelle")

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest _name rdir rfile efile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = _dir <//> efile
   in PosTest {..}

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Steps $ \step -> do
        entryPoint <- testDefaultEntryPointIO _dir _file
        step "Translate"
        PipelineResult {..} <- snd <$> testRunIO entryPoint upToIsabelle
        let thy = _pipelineResult ^. resultTheory
        step "Checking against expected output file"
        expFile :: Text <- readFile _expectedFile
        assertEqDiffText "Compare to expected output" (ppPrint thy <> "\n") expFile
    }

allTests :: TestTree
allTests =
  testGroup
    "Isabelle positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ posTest
      "Test Isabelle translation"
      $(mkRelDir ".")
      $(mkRelFile "Program.juvix")
      $(mkRelFile "isabelle/Program.thy")
  ]
