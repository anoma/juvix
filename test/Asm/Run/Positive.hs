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
      "out/test014.out"
  ]
