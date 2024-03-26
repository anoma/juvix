module Casm.Reg.Cairo where

import Base
import Casm.Reg.Base
import Casm.Reg.Positive qualified as P

testDescr :: P.PosTest -> TestDescr
testDescr P.PosTest {..} =
  let tRoot = P.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regToCairoAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg to Cairo translation positive tests"
    ( map (mkTest . testDescr) $
        P.filterOutTests
          [ "Test001: Arithmetic opcodes",
            "Test013: Fibonacci numbers in linear time",
            "Test014: Trees",
            "Test016: Arithmetic",
            "Test017: Closures as arguments",
            "Test023: McCarthy's 91 function",
            "Test024: Higher-order recursive functions",
            "Test027: Fast exponentiation",
            "Test030: Mutual recursion",
            "Test031: Temporary stack with branching",
            "Test036: Streams without memoization",
            "Test038: Apply & argsnum"
          ]
          P.tests
    )
