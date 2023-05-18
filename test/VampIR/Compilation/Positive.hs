module VampIR.Compilation.Positive where

import Base
import VampIR.Compilation.Base

data PosTest = PosTest
  { _paramsNum :: Int,
    _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _dataFile :: Path Abs File
  }

makeLenses ''PosTest

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/positive/Compilation")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  let tRoot = _dir
      file' = _file
      data' = _dataFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vampirCompileAssertion _paramsNum file' data'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix compilation pipeline positive tests"
    (map (mkTest . toTestDescr) tests)

posTest :: Int -> String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest _paramsNum _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _dataFile = root <//> routfile
   in PosTest {..}

tests :: [PosTest]
tests =
  [ posTest
      5
      "Test001: not function"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "data/test001.json"),
    posTest
      5
      "Test002: pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "data/test002.json"),
    posTest
      5
      "Test003: inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
      $(mkRelFile "data/test003.json"),
    posTest
      5
      "Test004: arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "data/test004.json")
  ]
