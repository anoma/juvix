module BackendGeb.Eval.Positive where

import BackendGeb.Eval.Base
import Base

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
              gebEvalAssertion file' expected'
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
      "Lambdas do not reduce"
      $(mkRelDir ".")
      $(mkRelFile "left-unit.geb")
      $(mkRelFile "Eval/out/left-unit.out"),
    PosTest
      "Test001.lisp. Reduce application of lambda"
      $(mkRelDir ".")
      $(mkRelFile "test001.lisp")
      $(mkRelFile "Eval/out/test001.out"),
    PosTest
      "Test002.lisp"
      $(mkRelDir ".")
      $(mkRelFile "test002.lisp")
      $(mkRelFile "Eval/out/test002.out"),
    PosTest
      "Test003.lisp"
      $(mkRelDir ".")
      $(mkRelFile "test003.lisp")
      $(mkRelFile "Eval/out/test003.out"),
    PosTest
      "Test005.lisp"
      $(mkRelDir ".")
      $(mkRelFile "test005.lisp")
      $(mkRelFile "Eval/out/test005.out")
  ]

-- PosTest
--   "IO builtins"
--   $(mkRelDir ".")
--   $(mkRelFile "test004.jvc")
--   $(mkRelFile "out/test004.out"),
-- PosTest
--   "Higher-order functions"
--   $(mkRelDir ".")
--   $(mkRelFile "test005.jvc")
--   $(mkRelFile "out/test005.out")
