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
    "JuvixGeb positive evaluation tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "App case on"
      $(mkRelDir ".")
      $(mkRelFile "app-case-on.geb")
      $(mkRelFile "Eval/out/app-case-on.out"),
    PosTest
      "App fst pair"
      $(mkRelDir ".")
      $(mkRelFile "app-fst-pair.geb")
      $(mkRelFile "Eval/out/app-fst-pair.out"),
    PosTest
      "lambda"
      $(mkRelDir ".")
      $(mkRelFile "lamb.geb")
      $(mkRelFile "Eval/out/lamb.out"),
    PosTest
      "App lambda"
      $(mkRelDir ".")
      $(mkRelFile "app-lambda.geb")
      $(mkRelFile "Eval/out/app-lambda.out"),
    PosTest
      "Double application"
      $(mkRelDir ".")
      $(mkRelFile "app-app-lambda.geb")
      $(mkRelFile "Eval/out/app-app-lambda.out"),
    PosTest
      "Basic app"
      $(mkRelDir ".")
      $(mkRelFile "basic-app.geb")
      $(mkRelFile "Eval/out/basic-app.out"),
    PosTest
      "case on"
      $(mkRelDir ".")
      $(mkRelFile "case-on.geb")
      $(mkRelFile "Eval/out/case-on.out")
  ]
