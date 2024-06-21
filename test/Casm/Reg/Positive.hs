module Casm.Reg.Positive where

import Base
import Casm.Reg.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File,
    _inputFile :: Maybe (Path Rel File)
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/Reg/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
      input' = fmap (tRoot <//>) _inputFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regToCasmAssertion file' input' expected'
        }

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests incl = filter (\PosTest {..} -> _name `notElem` incl)

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg to CASM translation positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: Arithmetic opcodes"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvr")
      $(mkRelFile "out/test001.out")
      Nothing,
    PosTest
      "Test002: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvr")
      $(mkRelFile "out/test002.out")
      Nothing,
    PosTest
      "Test003: Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvr")
      $(mkRelFile "out/test003.out")
      Nothing,
    PosTest
      "Test004: Tail calls"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvr")
      $(mkRelFile "out/test004.out")
      Nothing,
    PosTest
      "Test008: Branch"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvr")
      $(mkRelFile "out/test008.out")
      Nothing,
    PosTest
      "Test009: Case"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvr")
      $(mkRelFile "out/test009.out")
      Nothing,
    PosTest
      "Test010: Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvr")
      $(mkRelFile "out/test010.out")
      Nothing,
    PosTest
      "Test011: Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvr")
      $(mkRelFile "out/test011.out")
      Nothing,
    PosTest
      "Test012: Temporary stack"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvr")
      $(mkRelFile "out/test012.out")
      Nothing,
    PosTest
      "Test013: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvr")
      $(mkRelFile "out/test013.out")
      Nothing,
    PosTest
      "Test014: Trees"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvr")
      $(mkRelFile "out/test014.out")
      Nothing,
    PosTest
      "Test015: Functions returning functions"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvr")
      $(mkRelFile "out/test015.out")
      Nothing,
    PosTest
      "Test016: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test016.jvr")
      $(mkRelFile "out/test016.out")
      Nothing,
    PosTest
      "Test017: Closures as arguments"
      $(mkRelDir ".")
      $(mkRelFile "test017.jvr")
      $(mkRelFile "out/test017.out")
      Nothing,
    PosTest
      "Test018: Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test018.jvr")
      $(mkRelFile "out/test018.out")
      Nothing,
    PosTest
      "Test019: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test019.jvr")
      $(mkRelFile "out/test019.out")
      Nothing,
    PosTest
      "Test020: Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test020.jvr")
      $(mkRelFile "out/test020.out")
      Nothing,
    PosTest
      "Test021: Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test021.jvr")
      $(mkRelFile "out/test021.out")
      Nothing,
    PosTest
      "Test023: McCarthy's 91 function"
      $(mkRelDir ".")
      $(mkRelFile "test023.jvr")
      $(mkRelFile "out/test023.out")
      Nothing,
    PosTest
      "Test024: Higher-order recursive functions"
      $(mkRelDir ".")
      $(mkRelFile "test024.jvr")
      $(mkRelFile "out/test024.out")
      Nothing,
    PosTest
      "Test026: Currying & uncurrying"
      $(mkRelDir ".")
      $(mkRelFile "test026.jvr")
      $(mkRelFile "out/test026.out")
      Nothing,
    PosTest
      "Test027: Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test027.jvr")
      $(mkRelFile "out/test027.out")
      Nothing,
    PosTest
      "Test028: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test028.jvr")
      $(mkRelFile "out/test028.out")
      Nothing,
    PosTest
      "Test030: Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test030.jvr")
      $(mkRelFile "out/test030.out")
      Nothing,
    PosTest
      "Test031: Temporary stack with branching"
      $(mkRelDir ".")
      $(mkRelFile "test031.jvr")
      $(mkRelFile "out/test031.out")
      Nothing,
    PosTest
      "Test033: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test033.jvr")
      $(mkRelFile "out/test033.out")
      Nothing,
    PosTest
      "Test034: Higher-order function composition"
      $(mkRelDir ".")
      $(mkRelFile "test034.jvr")
      $(mkRelFile "out/test034.out")
      Nothing,
    PosTest
      "Test036: Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test036.jvr")
      $(mkRelFile "out/test036.out")
      Nothing,
    PosTest
      "Test038: Apply & argsnum"
      $(mkRelDir ".")
      $(mkRelFile "test038.jvr")
      $(mkRelFile "out/test038.out")
      Nothing,
    PosTest
      "Test039: Calls in a branch"
      $(mkRelDir ".")
      $(mkRelFile "test039.jvr")
      $(mkRelFile "out/test039.out")
      Nothing,
    PosTest
      "Test040: Input"
      $(mkRelDir ".")
      $(mkRelFile "test040.jvr")
      $(mkRelFile "out/test040.out")
      (Just $(mkRelFile "in/test040.json")),
    PosTest
      "Test043: Copy & constant propagation"
      $(mkRelDir ".")
      $(mkRelFile "test043.jvr")
      $(mkRelFile "out/test043.out")
      Nothing
  ]
