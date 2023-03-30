module BackendGeb.FromCore.Positive where

import BackendGeb.FromCore.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Geb/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion =
            Steps $
              coreToGebTranslationAssertion file' expected'
        }

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests out = filter (\PosTest {..} -> _name `notElem` out)

allTests :: TestTree
allTests =
  testGroup
    "JuvixGeb positive translation tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: not function"
      $(mkRelDir ".")
      $(mkRelFile "Core/test001.jvc")
      $(mkRelFile "Eval/out/test001.geb"),
    PosTest
      "Test002: pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "Core/test002.jvc")
      $(mkRelFile "Eval/out/test002.geb"),
    PosTest
      "Test003: inductive types"
      $(mkRelDir ".")
      $(mkRelFile "Core/test003.jvc")
      $(mkRelFile "Eval/out/test003.geb"),
    PosTest
      "Test004: definitions"
      $(mkRelDir ".")
      $(mkRelFile "Core/test004.jvc")
      $(mkRelFile "Eval/out/test004.geb"),
    PosTest
      "Test005: basic arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "Core/test005.jvc")
      $(mkRelFile "Eval/out/test005.geb"),
    PosTest
      "Test006: arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "Core/test006.jvc")
      $(mkRelFile "Eval/out/test006.geb"),
    PosTest
      "Test007: single-constructor inductive types"
      $(mkRelDir ".")
      $(mkRelFile "Core/test007.jvc")
      $(mkRelFile "Eval/out/test007.geb"),
    PosTest
      "Test008: higher-order inductive types"
      $(mkRelDir ".")
      $(mkRelFile "Core/test008.jvc")
      $(mkRelFile "Eval/out/test008.geb"),
    PosTest
      "Test009: comparisons"
      $(mkRelDir ".")
      $(mkRelFile "Core/test009.jvc")
      $(mkRelFile "Eval/out/test009.geb"),
    PosTest
      "Test010: let"
      $(mkRelDir ".")
      $(mkRelFile "Core/test010.jvc")
      $(mkRelFile "Eval/out/test010.geb"),
    PosTest
      "Test011: functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "Core/test011.jvc")
      $(mkRelFile "Eval/out/test011.geb"),
    PosTest
      "Test012: partial application"
      $(mkRelDir ".")
      $(mkRelFile "Core/test012.jvc")
      $(mkRelFile "Eval/out/test012.geb"),
    PosTest
      "Test013: mid-square hashing (unrolled)"
      $(mkRelDir ".")
      $(mkRelFile "Core/test013.jvc")
      $(mkRelFile "Eval/out/test013.geb"),
    PosTest
      "Test014: recursion"
      $(mkRelDir ".")
      $(mkRelFile "Core/test014.jvc")
      $(mkRelFile "Eval/out/test014.geb"),
    PosTest
      "Test015: tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "Core/test015.jvc")
      $(mkRelFile "Eval/out/test015.geb"),
    PosTest
      "Test016: tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "Core/test016.jvc")
      $(mkRelFile "Eval/out/test016.geb"),
    PosTest
      "Test017: recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "Core/test017.jvc")
      $(mkRelFile "Eval/out/test017.geb"),
    PosTest
      "Test018: tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "Core/test018.jvc")
      $(mkRelFile "Eval/out/test018.geb"),
    PosTest
      "Test019: higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "Core/test019.jvc")
      $(mkRelFile "Eval/out/test019.geb"),
    PosTest
      "Test020: McCarthy's 91 function"
      $(mkRelDir ".")
      $(mkRelFile "Core/test020.jvc")
      $(mkRelFile "Eval/out/test020.geb"),
    PosTest
      "Test021: fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "Core/test021.jvc")
      $(mkRelFile "Eval/out/test021.geb"),
    PosTest
      "Test022: mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "Core/test022.jvc")
      $(mkRelFile "Eval/out/test022.geb"),
    PosTest
      "Test023: Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "Core/test023.jvc")
      $(mkRelFile "Eval/out/test023.geb"),
    PosTest
      "Test024: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "Core/test024.jvc")
      $(mkRelFile "Eval/out/test024.geb"),
    PosTest
      "Test025: mid-square hashing"
      $(mkRelDir ".")
      $(mkRelFile "Core/test025.jvc")
      $(mkRelFile "Eval/out/test025.geb")
  ]
