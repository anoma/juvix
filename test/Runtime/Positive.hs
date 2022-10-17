module Runtime.Positive where

import Base
import Runtime.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _expectedFile :: FilePath
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/runtime/positive/"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ clangAssertion _file _expectedFile ""
        }

allTests :: TestTree
allTests =
  testGroup
    "Runtime positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "HelloWorld"
      "."
      "test001.c"
      "out/test001.out"
  ]
