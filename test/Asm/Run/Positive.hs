module Asm.Run.Positive where

import Asm.Run.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _expectedFile :: FilePath
  }

root :: FilePath
root = "tests/Asm/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertion _file _expectedFile
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
      "."
      "test001.jva"
      "out/test001.out",
    PosTest
      "Direct call"
      "."
      "test002.jva"
      "out/test002.out",
    PosTest
      "Indirect call"
      "."
      "test003.jva"
      "out/test003.out",
    PosTest
      "Tail calls"
      "."
      "test004.jva"
      "out/test004.out",
    PosTest
      "Tracing IO"
      "."
      "test005.jva"
      "out/test005.out",
    PosTest
      "IO builtins"
      "."
      "test006.jva"
      "out/test006.out",
    PosTest
      "Higher-order functions"
      "."
      "test007.jva"
      "out/test007.out",
    PosTest
      "Branch"
      "."
      "test008.jva"
      "out/test008.out",
    PosTest
      "Case"
      "."
      "test009.jva"
      "out/test009.out",
    PosTest
      "Recursion"
      "."
      "test010.jva"
      "out/test010.out",
    PosTest
      "Tail recursion"
      "."
      "test011.jva"
      "out/test011.out",
    PosTest
      "Temporary stack"
      "."
      "test012.jva"
      "out/test012.out",
    PosTest
      "Fibonacci numbers in linear time"
      "."
      "test013.jva"
      "out/test013.out",
    PosTest
      "Trees"
      "."
      "test014.jva"
      "out/test014.out",
    PosTest
      "Functions returning functions"
      "."
      "test015.jva"
      "out/test015.out",
    PosTest
      "Arithmetic"
      "."
      "test016.jva"
      "out/test016.out",
    PosTest
      "Closures as arguments"
      "."
      "test017.jva"
      "out/test017.out",
    PosTest
      "Closure extension"
      "."
      "test018.jva"
      "out/test018.out",
    PosTest
      "Recursion through higher-order functions"
      "."
      "test019.jva"
      "out/test019.out",
    PosTest
      "Tail recursion through higher-order functions"
      "."
      "test020.jva"
      "out/test020.out",
    PosTest
      "Higher-order functions and recursion"
      "."
      "test021.jva"
      "out/test021.out",
    PosTest
      "Self-application"
      "."
      "test022.jva"
      "out/test022.out",
    PosTest
      "McCarthy's 91 function"
      "."
      "test023.jva"
      "out/test023.out",
    PosTest
      "Higher-order recursive functions"
      "."
      "test024.jva"
      "out/test024.out",
    PosTest
      "Dynamic closure extension"
      "."
      "test025.jva"
      "out/test025.out",
    PosTest
      "Currying & uncurrying"
      "."
      "test026.jva"
      "out/test026.out",
    PosTest
      "Fast exponentiation"
      "."
      "test027.jva"
      "out/test027.out",
    PosTest
      "Lists"
      "."
      "test028.jva"
      "out/test028.out",
    PosTest
      "Structural equality"
      "."
      "test029.jva"
      "out/test029.out",
    PosTest
      "Mutual recursion"
      "."
      "test030.jva"
      "out/test030.out",
    PosTest
      "Temporary stack with branching"
      "."
      "test031.jva"
      "out/test031.out",
    PosTest
      "Church numerals"
      "."
      "test032.jva"
      "out/test032.out"
  ]
