module Casm.Reg.Positive where

import Base
import Casm.Reg.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/Reg/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regToCasmAssertion file' expected'
        }

filterTests :: [String] -> [PosTest] -> [PosTest]
filterTests incl = filter (\PosTest {..} -> _name `elem` incl)

allTests :: TestTree
allTests =
  testGroup
    "CASM from JuvixReg translation positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: Arithmetic opcodes"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvr")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Test002: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvr")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Test003: Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvr")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Test004: Tail calls"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvr")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Test008: Branch"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvr")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Test009: Case"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvr")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Test010: Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvr")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Test011: Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvr")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Test012: Temporary stack"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvr")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Test013: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvr")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Test014: Trees"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvr")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Test015: Functions returning functions"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvr")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Test016: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test016.jvr")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Test017: Closures as arguments"
      $(mkRelDir ".")
      $(mkRelFile "test017.jvr")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Test018: Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test018.jvr")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Test019: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test019.jvr")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Test020: Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test020.jvr")
      $(mkRelFile "out/test020.out"),
    PosTest
      "Test021: Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test021.jvr")
      $(mkRelFile "out/test021.out"),
    PosTest
      "Test023: McCarthy's 91 function"
      $(mkRelDir ".")
      $(mkRelFile "test023.jvr")
      $(mkRelFile "out/test023.out"),
    PosTest
      "Test024: Higher-order recursive functions"
      $(mkRelDir ".")
      $(mkRelFile "test024.jvr")
      $(mkRelFile "out/test024.out"),
    PosTest
      "Test026: Currying & uncurrying"
      $(mkRelDir ".")
      $(mkRelFile "test026.jvr")
      $(mkRelFile "out/test026.out"),
    PosTest
      "Test027: Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test027.jvr")
      $(mkRelFile "out/test027.out"),
    PosTest
      "Test028: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test028.jvr")
      $(mkRelFile "out/test028.out"),
    PosTest
      "Test030: Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test030.jvr")
      $(mkRelFile "out/test030.out"),
    PosTest
      "Test031: Temporary stack with branching"
      $(mkRelDir ".")
      $(mkRelFile "test031.jvr")
      $(mkRelFile "out/test031.out"),
    PosTest
      "Test033: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test033.jvr")
      $(mkRelFile "out/test033.out"),
    PosTest
      "Test034: Higher-order function composition"
      $(mkRelDir ".")
      $(mkRelFile "test034.jvr")
      $(mkRelFile "out/test034.out"),
    PosTest
      "Test036: Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test036.jvr")
      $(mkRelFile "out/test036.out"),
    PosTest
      "Test038: Apply & argsnum"
      $(mkRelDir ".")
      $(mkRelFile "test038.jvr")
      $(mkRelFile "out/test038.out")
  ]
