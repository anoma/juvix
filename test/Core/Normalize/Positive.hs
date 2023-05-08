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
      $(mkRelFile "data/test002.json"),
    PosTest
      "Test003: inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "data/test003.json"),
    PosTest
      "Test004: definitions"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "data/test004.json"),
    PosTest
      "Test005: basic arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc")
      $(mkRelFile "data/test005.json"),
    PosTest
      "Test006: arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvc")
      $(mkRelFile "data/test006.json"),
    PosTest
      "Test007: single-constructor inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvc")
      $(mkRelFile "data/test007.json"),
    PosTest
      "Test008: higher-order inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvc")
      $(mkRelFile "data/test008.json"),
    PosTest
      "Test009: comparisons"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvc")
      $(mkRelFile "data/test009.json"),
    PosTest
      "Test010: let"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "data/test010.json")
  ]
