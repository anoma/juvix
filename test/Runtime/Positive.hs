module Runtime.Positive where

import Base
import Runtime.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/runtime/positive/")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ clangAssertion file' expected' ""
        }

allTests :: TestTree
allTests =
  testGroup
    "Runtime positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: HelloWorld"
      $(mkRelDir ".")
      $(mkRelFile "test001.c")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Test002: Page allocation"
      $(mkRelDir ".")
      $(mkRelFile "test002.c")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Test003: Printing of integers"
      $(mkRelDir ".")
      $(mkRelFile "test003.c")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Test004: Allocator for unstructured objects"
      $(mkRelDir ".")
      $(mkRelFile "test004.c")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Test005: Allocator for unstructured objects (macro API)"
      $(mkRelDir ".")
      $(mkRelFile "test005.c")
      $(mkRelFile "out/test005.out"),
    PosTest
      "Test006: Stack"
      $(mkRelDir ".")
      $(mkRelFile "test006.c")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Test007: Prologue and epilogue"
      $(mkRelDir ".")
      $(mkRelFile "test007.c")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Test008: Basic arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test008.c")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Test009: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test009.c")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Test010: Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test010.c")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Test011: Tail calls"
      $(mkRelDir ".")
      $(mkRelFile "test011.c")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Test012: Tracing and strings"
      $(mkRelDir ".")
      $(mkRelFile "test012.c")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Test013: IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test013.c")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Test014: Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test014.c")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Test015: Branching, matching and recursion on lists"
      $(mkRelDir ".")
      $(mkRelFile "test015.c")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Test016: Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test016.c")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Test017: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.c")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Test018: Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test018.c")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Test019: Dynamic closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test019.c")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Test020: Higher-order function composition"
      $(mkRelDir ".")
      $(mkRelFile "test020.c")
      $(mkRelFile "out/test020.out")
  ]
