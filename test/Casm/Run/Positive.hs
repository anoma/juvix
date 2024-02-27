module Casm.Run.Positive where

import Base
import Casm.Run.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ casmRunAssertion file' expected'
        }

filterTests :: [String] -> [PosTest] -> [PosTest]
filterTests incl = filter (\PosTest {..} -> _name `elem` incl)

allTests :: TestTree
allTests =
  testGroup
    "CASM run positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: Sum of numbers"
      $(mkRelDir ".")
      $(mkRelFile "test001.casm")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Test002: Factorial"
      $(mkRelDir ".")
      $(mkRelFile "test002.casm")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Test003: Direct call"
      $(mkRelDir ".")
      $(mkRelFile "test003.casm")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Test004: Indirect call"
      $(mkRelDir ".")
      $(mkRelFile "test004.casm")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Test005: Exp function"
      $(mkRelDir ".")
      $(mkRelFile "test005.casm")
      $(mkRelFile "out/test005.out"),
    PosTest
      "Test006: Branch"
      $(mkRelDir ".")
      $(mkRelFile "test006.casm")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Test007: Closure extension"
      $(mkRelDir ".")
      $(mkRelFile "test007.casm")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Test008: Integer arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test008.casm")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Test009: Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.casm")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Test010: Functions returning functions"
      $(mkRelDir ".")
      $(mkRelFile "test010.casm")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Test011: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test011.casm")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Test012: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test012.casm")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Test013: Currying and uncurrying"
      $(mkRelDir ".")
      $(mkRelFile "test013.casm")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Test014: Field arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.casm")
      $(mkRelFile "out/test014.out")
  ]
