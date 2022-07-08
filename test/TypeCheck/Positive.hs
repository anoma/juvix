module TypeCheck.Positive where

import Base
import Juvix.Pipeline

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
      "Simple.juvix",
    PosTest
      "Literal String matches any type"
      "MicroJuvix"
      "LiteralString.juvix",
    PosTest
      "Box type"
      "MicroJuvix"
      "Box.juvix",
    PosTest
      "Literal Int matches any type"
      "MicroJuvix"
      "LiteralInt.juvix",
    PosTest
      "GHC backend Hello World"
      "MiniHaskell"
      "HelloWorld.juvix",
    PosTest
      "PolySimpleFungibleToken"
      "FullExamples"
      "SimpleFungibleTokenImplicit.juvix",
    PosTest
      "GHC backend MonoSimpleFungibleToken"
      "FullExamples"
      "MonoSimpleFungibleToken.juvix",
    PosTest
      "Axiom"
      "."
      "Axiom.juvix",
    PosTest
      "Inductive"
      "."
      "Inductive.juvix",
    PosTest
      "Operators"
      "."
      "Operators.juvix",
    PosTest
      "Holes in type signature"
      "MicroJuvix"
      "HoleInSignature.juvix",
    PosTest
      "Polymorphism and higher rank functions"
      "."
      "Polymorphism.juvix",
    PosTest
      "Polymorphism and higher rank functions with explicit holes"
      "."
      "PolymorphismHoles.juvix",
    PosTest
      "Implicit arguments"
      "MicroJuvix"
      "Implicit.juvix",
    PosTest
      "Import a builtin multiple times"
      "BuiltinsMultiImport"
      "Input.juvix",
    PosTest
      "open import a builtin multiple times"
      "BuiltinsMultiOpenImport"
      "Input.juvix"
  ]
