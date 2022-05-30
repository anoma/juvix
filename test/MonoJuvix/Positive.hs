module MonoJuvix.Positive where

import Base
import MiniJuvix.Pipeline

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath
  }

root :: FilePath
root = "tests/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint _file
            (void . runIO) (upToMonoJuvix entryPoint)
        }

allTests :: TestTree
allTests =
  testGroup
    "MonoJuvix positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "General example"
      "."
      "Polymorphism.mjuvix",
    PosTest
      "Polymorphic Simple Fungible Token"
      "FullExamples"
      "PolySimpleFungibleToken.mjuvix"
  ]
