module Examples.Positive where

import Base
import Compilation.Base

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File,
    _stdin :: Text
  }

makeLenses ''PosTest

srcRoot :: Path Abs Dir
srcRoot = relToProject $(mkRelDir "examples/")

expectedRoot :: Path Abs Dir
expectedRoot = relToProject $(mkRelDir "tests/examplesExpected")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  TestDescr
    { _testRoot = _dir,
      _testName = _name,
      _testAssertion = Steps $ compileAssertion (CompileOnly _stdin) _file _expectedFile
    }

allTests :: TestTree
allTests =
  testGroup
    "Compile and test examples"
    (map (mkTest . toTestDescr) tests)

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> Text -> PosTest
posTest _name rdir rfile routfile _stdin =
  let _dir = srcRoot <//> rdir
      _file = _dir <//> rfile
      _expectedFile = expectedRoot <//> routfile
   in PosTest {..}

tests :: [PosTest]
tests =
  [ posTest
      "Fibonacci"
      $(mkRelDir "milestone/Fibonacci")
      $(mkRelFile "Fibonacci.juvix")
      $(mkRelFile "Fibonacci/expected.golden")
      "25\n",
    posTest
      "Towers of Hanoi"
      $(mkRelDir "milestone/Hanoi")
      $(mkRelFile "Hanoi.juvix")
      $(mkRelFile "Hanoi/expected.golden")
      ""
  ]
