module TypeCheck.Positive where

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
            (void . runIO) (upToMicroJuvixTyped entryPoint)
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Simple"
      "MicroJuvix"
      "Simple.mjuvix",
    PosTest
      "Literal String matches any type"
      "MicroJuvix"
      "LiteralString.mjuvix",
    PosTest
      "Literal Int matches any type"
      "MicroJuvix"
      "LiteralInt.mjuvix",
    PosTest
      "GHC backend Hello World"
      "MiniHaskell"
      "HelloWorld.mjuvix",
    PosTest
      "PolySimpleFungibleToken"
      "FullExamples"
      "SimpleFungibleTokenImplicit.mjuvix",
    PosTest
      "GHC backend MonoSimpleFungibleToken"
      "FullExamples"
      "MonoSimpleFungibleToken.mjuvix",
    PosTest
      "Axiom"
      "."
      "Axiom.mjuvix",
    PosTest
      "Inductive"
      "."
      "Inductive.mjuvix",
    PosTest
      "Operators"
      "."
      "Operators.mjuvix",
    PosTest
      "Holes in type signature"
      "MicroJuvix"
      "HoleInSignature.mjuvix",
    PosTest
      "Polymorphism and higher rank functions"
      "."
      "Polymorphism.mjuvix",
    PosTest
      "Polymorphism and higher rank functions with explicit holes"
      "."
      "PolymorphismHoles.mjuvix",
    PosTest
      "Implicit arguments"
      "MicroJuvix"
      "Implicit.mjuvix"
  ]
