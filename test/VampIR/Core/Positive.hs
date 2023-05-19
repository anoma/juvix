module VampIR.Core.Positive where

import Base
import VampIR.Core.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _dataFile :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/positive/translation")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      data' = tRoot <//> _dataFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vampirAssertion file' data'
        }

allTests :: TestTree
allTests =
  testGroup
    "VampIR translation positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      "Test002"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json")
  ]
