module TypeCheck.Positive where

import Base
import Juvix.Pipeline
import TypeCheck.Negative qualified as N

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

--------------------------------------------------------------------------------
-- Testing --no-positivity flag with all related negative tests
--------------------------------------------------------------------------------

rootNegTests :: FilePath
rootNegTests = "tests/negative/"

testNoPositivityFlag :: N.NegTest -> TestDescr
testNoPositivityFlag N.NegTest {..} =
  let tRoot = rootNegTests </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint =
                  EntryPoint
                    { _entryPointRoot = ".",
                      _entryPointNoTermination = False,
                      _entryPointNoPositivity = True,
                      _entryPointNoStdlib = False,
                      _entryPointPackage = emptyPackage,
                      _entryPointModulePaths = pure _file
                    }

            (void . runIO) (upToMicroJuvix entryPoint)
        }

negPositivityTests :: [N.NegTest]
negPositivityTests = N.negPositivityTests

testPositivityKeyword :: [PosTest]
testPositivityKeyword =
  [ PosTest
      "Mark T0 data type as strictly positive"
      "MicroJuvix/Positivity"
      "E5.juvix"
  ]

positivityTestGroup :: TestTree
positivityTestGroup =
  testGroup
    "Positive tests for the positivity condition"
    [ testGroup
        "Bypass positivity checking using --non-positivity flag on negative tests"
        (map (mkTest . testNoPositivityFlag) negPositivityTests),
      testGroup
        "Usages of the positive keyword"
        (map (mkTest . testDescr) testPositivityKeyword)
    ]

--------------------------------------------------------------------------------

allTests :: TestTree
allTests =
  testGroup
    "Typecheck positive tests"
    [ testGroup
        "General typechecking tests"
        (map (mkTest . testDescr) tests),
      positivityTestGroup
    ]

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
      "Simple type alias"
      "."
      "TypeAlias.juvix",
    PosTest
      "Refine hole in type signature"
      "272"
      "M.juvix",
    PosTest
      "Pattern match a hole type"
      "265"
      "M.juvix",
    PosTest
      "Import a builtin multiple times"
      "BuiltinsMultiImport"
      "Input.juvix",
    PosTest
      "open import a builtin multiple times"
      "BuiltinsMultiOpenImport"
      "Input.juvix"
  ]
