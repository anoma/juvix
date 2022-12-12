module Asm.Validate.Negative where

import Asm.Validate.Base
import Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Asm/negative")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmValidateErrorAssertion file'
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
      $(mkRelDir ".")
      $(mkRelFile "vtest001.jva"),
    NegTest
      "Arithmetic type mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest002.jva"),
    NegTest
      "Function type mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest003.jva"),
    NegTest
      "Not enough function arguments"
      $(mkRelDir ".")
      $(mkRelFile "vtest004.jva"),
    NegTest
      "Missing return"
      $(mkRelDir ".")
      $(mkRelFile "vtest005.jva"),
    NegTest
      "Branch stack height mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest006.jva"),
    NegTest
      "Branch type mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest007.jva"),
    NegTest
      "Case stack height mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest008.jva"),
    NegTest
      "Case type mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest009.jva"),
    NegTest
      "Value stack type mismatch"
      $(mkRelDir ".")
      $(mkRelFile "vtest010.jva")
  ]
