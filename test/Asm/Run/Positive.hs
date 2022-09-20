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
      "If-then-else"
      "."
      "test008.jva"
      "out/test008.out"
  ]
