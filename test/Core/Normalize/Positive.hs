module Core.Normalize.Positive where

import Base
import Core.Normalize.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

makeLenses ''PosTest

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/positive/Core")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreNormalizeAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore positive tests"
    (map (mkTest . toTestDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: not function"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      "Test002: pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json")
  ]
