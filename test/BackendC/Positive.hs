module BackendC.Positive where

import BackendC.Base
import Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _stdlibMode :: StdlibMode
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive/MiniC"

mainFile :: FilePath
mainFile = "Input.juvix"

expectedFile :: FilePath
expectedFile = "expected.golden"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ clangAssertion _stdlibMode mainFile expectedFile ""
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "HelloWorld" "HelloWorld" StdlibExclude,
    PosTest "Inductive types and pattern matching" "Nat" StdlibExclude,
    PosTest "Polymorphic types" "Polymorphism" StdlibExclude,
    PosTest "Polymorphic axioms" "PolymorphicAxioms" StdlibExclude,
    PosTest "Polymorphic target" "PolymorphicTarget" StdlibExclude,
    PosTest "Multiple modules" "MultiModules" StdlibExclude,
    PosTest "Higher Order Functions" "HigherOrder" StdlibExclude,
    PosTest "Higher Order Functions and explicit holes" "PolymorphismHoles" StdlibExclude,
    PosTest "Closures with no environment" "ClosureNoEnv" StdlibExclude,
    PosTest "Closures with environment" "ClosureEnv" StdlibExclude,
    PosTest "SimpleFungibleTokenImplicit" "SimpleFungibleTokenImplicit" StdlibExclude,
    PosTest "Mutually recursive function" "MutuallyRecursive" StdlibExclude,
    PosTest "Nested List type" "NestedList" StdlibExclude,
    PosTest "Builtin types and functions" "Builtins" StdlibExclude,
    PosTest "Import from embedded standard library" "StdlibImport" StdlibInclude
  ]
