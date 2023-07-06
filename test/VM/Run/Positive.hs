module VM.Run.Positive where

import Base
import VM.Run.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _dataFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VM/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      data' = tRoot <//> _dataFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vmRunAssertion file' data'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixVM run positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvb")
      $(mkRelFile "data/test001.json"),
    PosTest
      "Test002: Loops"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvb")
      $(mkRelFile "data/test002.json"),
    PosTest
      "Test003: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvb")
      $(mkRelFile "data/test003.json"),
    PosTest
      "Test004: Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvb")
      $(mkRelFile "data/test004.json"),
    PosTest
      "Test005: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvb")
      $(mkRelFile "data/test005.json"),
    PosTest
      "Test006: Comparisons"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvb")
      $(mkRelFile "data/test006.json")
  ]
