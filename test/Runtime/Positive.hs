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
      "HelloWorld"
      $(mkRelDir ".")
      $(mkRelFile "test001.c")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Page allocation"
      $(mkRelDir ".")
      $(mkRelFile "test002.c")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Printing of integers"
      $(mkRelDir ".")
      $(mkRelFile "test003.c")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Allocator for unstructured objects"
      $(mkRelDir ".")
      $(mkRelFile "test004.c")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Allocator for unstructured objects (macro API)"
      $(mkRelDir ".")
      $(mkRelFile "test005.c")
      $(mkRelFile "out/test005.out"),
    PosTest
      "Stack"
      $(mkRelDir ".")
      $(mkRelFile "test006.c")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Prologue and epilogue"
      $(mkRelDir ".")
      $(mkRelFile "test007.c")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Basic arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test008.c")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test009.c")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test010.c")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Tail calls"
      $(mkRelDir ".")
      $(mkRelFile "test011.c")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Tracing and strings"
      $(mkRelDir ".")
      $(mkRelFile "test012.c")
      $(mkRelFile "out/test012.out"),
    PosTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test013.c")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test014.c")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Branching, matching and recursion on lists"
      $(mkRelDir ".")
      $(mkRelFile "test015.c")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test016.c")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.c")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test018.c")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Dynamic closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test019.c")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Higher-order function composition"
      $(mkRelDir ".")
      $(mkRelFile "test020.c")
      $(mkRelFile "out/test020.out")
  ]
