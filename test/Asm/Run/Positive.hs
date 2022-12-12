module Asm.Run.Positive where

import Asm.Run.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Asm/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' =  tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertion file' expected' return (const (return ()))
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixAsm run positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Arithmetic opcodes"
      $(mkRelDir ".")
      $(mkRelFile "test001.jva")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test002.jva")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test003.jva")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Tail calls"
      $(mkRelDir ".")
      $(mkRelFile "test004.jva")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Tracing IO"
      $(mkRelDir ".")
      $(mkRelFile "test005.jva")
      $(mkRelFile "out/test005.out"),
    PosTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test006.jva")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test007.jva")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Branch"
      $(mkRelDir ".")
      $(mkRelFile "test008.jva")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Case"
      $(mkRelDir ".")
      $(mkRelFile "test009.jva")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test010.jva")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test011.jva")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Temporary stack"
      $(mkRelDir ".")
      $(mkRelFile "test012.jva")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test013.jva")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Trees"
      $(mkRelDir ".")
      $(mkRelFile "test014.jva")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Functions returning functions"
      $(mkRelDir ".")
      $(mkRelFile "test015.jva")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test016.jva")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Closures as arguments"
      $(mkRelDir ".")
      $(mkRelFile "test017.jva")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test018.jva")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test019.jva")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test020.jva")
      $(mkRelFile "out/test020.out"),
    PosTest
      "Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test021.jva")
      $(mkRelFile "out/test021.out"),
    PosTest
      "Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test022.jva")
      $(mkRelFile "out/test022.out"),
    PosTest
      "McCarthy's 91 function"
      $(mkRelDir ".")
      $(mkRelFile "test023.jva")
      $(mkRelFile "out/test023.out"),
    PosTest
      "Higher-order recursive functions"
      $(mkRelDir ".")
      $(mkRelFile "test024.jva")
      $(mkRelFile "out/test024.out"),
    PosTest
      "Dynamic closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test025.jva")
      $(mkRelFile "out/test025.out"),
    PosTest
      "Currying & uncurrying"
      $(mkRelDir ".")
      $(mkRelFile "test026.jva")
      $(mkRelFile "out/test026.out"),
    PosTest
      "Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test027.jva")
      $(mkRelFile "out/test027.out"),
    PosTest
      "Lists"
      $(mkRelDir ".")
      $(mkRelFile "test028.jva")
      $(mkRelFile "out/test028.out"),
    PosTest
      "Structural equality"
      $(mkRelDir ".")
      $(mkRelFile "test029.jva")
      $(mkRelFile "out/test029.out"),
    PosTest
      "Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test030.jva")
      $(mkRelFile "out/test030.out"),
    PosTest
      "Temporary stack with branching"
      $(mkRelDir ".")
      $(mkRelFile "test031.jva")
      $(mkRelFile "out/test031.out"),
    PosTest
      "Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test032.jva")
      $(mkRelFile "out/test032.out"),
    PosTest
      "Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test033.jva")
      $(mkRelFile "out/test033.out"),
    PosTest
      "Higher-order function composition"
      $(mkRelDir ".")
      $(mkRelFile "test034.jva")
      $(mkRelFile "out/test034.out"),
    PosTest
      "Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test035.jva")
      $(mkRelFile "out/test035.out"),
    PosTest
      "Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test036.jva")
      $(mkRelFile "out/test036.out")
  ]
