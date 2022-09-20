module Asm.Validate.Negative where

import Asm.Validate.Base
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
          _testAssertion = Steps $ asmValidateErrorAssertion _file
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixAsm validate negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Wrong stack height on exit"
      "."
      "vtest001.jva",
    NegTest
      "Arithmetic type mismatch"
      "."
      "vtest002.jva",
    NegTest
      "Function type mismatch"
      "."
      "vtest003.jva",
    NegTest
      "Not enough function arguments"
      "."
      "vtest004.jva",
    NegTest
      "Missing return"
      "."
      "vtest005.jva"
  ]
