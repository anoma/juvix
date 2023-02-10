module Geb.Eval.Positive where

import Base
import Geb.Eval.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Geb/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion =
            Steps $
              gebEvalAssertion file' expected' [] (const (return ()))
        }

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests out = filter (\PosTest {..} -> _name `notElem` out)

allTests :: TestTree
allTests =
  testGroup
    "JuvixGeb positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Empty program with comments"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "out/test003.out"),
    PosTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc")
      $(mkRelFile "out/test005.out")
  ]
