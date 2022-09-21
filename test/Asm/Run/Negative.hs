module Asm.Run.Negative where

import Asm.Run.Base
import Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath
  }

root :: FilePath
root = "tests/Asm/negative"

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunErrorAssertion _file
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixAsm run negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Division by zero"
      "."
      "test001.jva",
    NegTest
      "Invalid memory access"
      "."
      "test002.jva",
    NegTest
      "No matching case branch"
      "."
      "test003.jva"
  ]
