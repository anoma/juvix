module VM.Compile.Positive where

import Base
import Juvix.Compiler.VM.Options qualified as VM
import VM.Compile.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _dataFile :: Path Rel File,
    _stackSize :: Int,
    _heapSize :: Int,
    _stepsNum :: Int
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VM/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      data' = tRoot <//> _dataFile
      opts =
        VM.defaultOptions
          { VM._optStackSize = _stackSize,
            VM._optHeapSize = _heapSize,
            VM._optStepsNum = _stepsNum
          }
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vmCompileAssertion opts file' data'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixVM compile positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvb")
      $(mkRelFile "data/test001.json")
      0
      0
      15,
    {-    PosTest
          "Test002: Loops"
          $(mkRelDir ".")
          $(mkRelFile "test002.jvb")
          $(mkRelFile "data/test002.json")
          0
          0
          100, -}
    PosTest
      "Test003: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvb")
      $(mkRelFile "data/test003.json")
      10
      0
      15,
    {-    PosTest
          "Test004: Indirect call"
          $(mkRelDir ".")
          $(mkRelFile "test004.jvb")
          $(mkRelFile "data/test004.json")
          10
          10
          70, -}
    PosTest
      "Test006: Comparisons"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvb")
      $(mkRelFile "data/test006.json")
      0
      0
      15
  ]
