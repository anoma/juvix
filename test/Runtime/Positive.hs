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
      "out/test001.out",
    PosTest
      "Page allocation"
      "."
      "test002.c"
      "out/test002.out",
    PosTest
      "Printing of integers"
      "."
      "test003.c"
      "out/test003.out",
    PosTest
      "Allocator for unstructured objects"
      "."
      "test004.c"
      "out/test004.out",
    PosTest
      "Allocator for unstructured objects (macro API)"
      "."
      "test005.c"
      "out/test005.out",
    PosTest
      "Stack"
      "."
      "test006.c"
      "out/test006.out",
    PosTest
      "Prologue and epilogue"
      "."
      "test007.c"
      "out/test007.out",
    PosTest
      "Basic arithmetic"
      "."
      "test008.c"
      "out/test008.out",
    PosTest
      "Direct call"
      "."
      "test009.c"
      "out/test009.out",
    PosTest
      "Indirect call"
      "."
      "test010.c"
      "out/test010.out",
    PosTest
      "Tail calls"
      "."
      "test011.c"
      "out/test011.out",
    PosTest
      "Tracing and strings"
      "."
      "test012.c"
      "out/test012.out",
    PosTest
      "IO builtins"
      "."
      "test013.c"
      "out/test013.out",
    PosTest
      "Higher-order functions"
      "."
      "test014.c"
      "out/test014.out",
    PosTest
      "Branching, matching and recursion on lists"
      "."
      "test015.c"
      "out/test015.out",
    PosTest
      "Closure extension"
      "."
      "test016.c"
      "out/test016.out",
    PosTest
      "Recursion through higher-order functions"
      "."
      "test017.c"
      "out/test017.out",
    PosTest
      "Tail recursion through higher-order functions"
      "."
      "test018.c"
      "out/test018.out",
    PosTest
      "Dynamic closure extension"
      "."
      "test019.c"
      "out/test019.out",
    PosTest
      "Higher-order function composition"
      "."
      "test020.c"
      "out/test020.out"
  ]
