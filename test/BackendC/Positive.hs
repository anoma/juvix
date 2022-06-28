module BackendC.Positive where

import BackendC.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive/MiniC"

mainFile :: FilePath
mainFile = "Input.mjuvix"

expectedFile :: FilePath
expectedFile = "expected.golden"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ clangAssertion mainFile expectedFile ""
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "HelloWorld" "HelloWorld",
    PosTest "Inductive types and pattern matching" "Nat",
    PosTest "Polymorphic types" "Polymorphism",
    PosTest "Multiple modules" "MultiModules",
    PosTest "Higher Order Functions" "HigherOrder",
    PosTest "Higher Order Functions and explicit holes" "PolymorphismHoles",
    PosTest "Closures with no environment" "ClosureNoEnv",
    PosTest "Closures with environment" "ClosureEnv",
    PosTest "SimpleFungibleTokenImplicit" "SimpleFungibleTokenImplicit",
    PosTest "Mutually recursive function" "MutuallyRecursive",
    PosTest "Nested List type" "NestedList",
    PosTest "Builtin types and functions" "Builtins"
  ]
