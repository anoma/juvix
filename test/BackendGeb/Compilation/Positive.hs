module BackendGeb.Compilation.Positive where

import BackendGeb.Compilation.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Geb/positive/Compilation")

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
              gebCompilationAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixGeb positive compilation tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: not function"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "out/test001.geb"),
    PosTest
      "Test002: pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "out/test002.geb"),
    PosTest
      "Test003: inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
      $(mkRelFile "out/test003.geb"),
    PosTest
      "Test004: definitions"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "out/test004.geb"),
    PosTest
      "Test005: basic arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix")
      $(mkRelFile "out/test005.geb"),
    PosTest
      "Test006: arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "out/test006.geb"),
    PosTest
      "Test007: single-constructor inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix")
      $(mkRelFile "out/test007.geb"),
    PosTest
      "Test008: higher-order inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix")
      $(mkRelFile "out/test008.geb"),
    PosTest
      "Test009: let"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix")
      $(mkRelFile "out/test009.geb"),
    PosTest
      "Test010: functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix")
      $(mkRelFile "out/test010.geb"),
    PosTest
      "Test011: applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "out/test011.geb"),
    PosTest
      "Test012: mid-square hashing (unrolled)"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix")
      $(mkRelFile "out/test012.geb"),
    PosTest
      "Test013: recursion"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix")
      $(mkRelFile "out/test013.geb"),
    PosTest
      "Test014: tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
      $(mkRelFile "out/test014.geb"),
    PosTest
      "Test015: tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test015.juvix")
      $(mkRelFile "out/test015.geb"),
    PosTest
      "Test016: local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test016.juvix")
      $(mkRelFile "out/test016.geb"),
    PosTest
      "Test017: recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.juvix")
      $(mkRelFile "out/test017.geb"),
    PosTest
      "Test018: tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test018.juvix")
      $(mkRelFile "out/test018.geb"),
    PosTest
      "Test019: higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test019.juvix")
      $(mkRelFile "out/test019.geb"),
    PosTest
      "Test020: McCarthy's 91 function"
      $(mkRelDir ".")
      $(mkRelFile "test020.juvix")
      $(mkRelFile "out/test020.geb"),
    PosTest
      "Test021: fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test021.juvix")
      $(mkRelFile "out/test021.geb"),
    PosTest
      "Test022: mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test022.juvix")
      $(mkRelFile "out/test022.geb"),
    PosTest
      "Test023: Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test023.juvix")
      $(mkRelFile "out/test023.geb"),
    PosTest
      "Test024: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test024.juvix")
      $(mkRelFile "out/test024.geb"),
    PosTest
      "Test025: mid-square hashing"
      $(mkRelDir ".")
      $(mkRelFile "test025.juvix")
      $(mkRelFile "out/test025.geb")
  ]
