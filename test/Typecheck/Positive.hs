module Typecheck.Positive where

import Base
import Juvix.Compiler.Pipeline
import Typecheck.Negative qualified as N

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
            (void . runIO' entryPoint) upToInternalTyped
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
                  (defaultEntryPoint _file)
                    { _entryPointNoPositivity = True
                    }

            (void . runIO' entryPoint) upToInternal
        }

negPositivityTests :: [N.NegTest]
negPositivityTests = N.negPositivityTests

testPositivityKeyword :: [PosTest]
testPositivityKeyword =
  [ PosTest
      "Mark T0 data type as strictly positive"
      "Internal/Positivity"
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
      "Internal"
      "Simple.juvix",
    PosTest
      "Literal String matches any type"
      "Internal"
      "LiteralString.juvix",
    PosTest
      "Box type"
      "Internal"
      "Box.juvix",
    PosTest
      "Literal Int matches any type"
      "Internal"
      "LiteralInt.juvix",
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
      "Internal"
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
      "Internal"
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
      "Pattern match type synonym"
      "issue1466"
      "M.juvix",
    PosTest
      "Import a builtin multiple times"
      "BuiltinsMultiImport"
      "Input.juvix",
    PosTest
      "Basic lambda functions"
      "Internal"
      "Lambda.juvix",
    PosTest
      "Simple mutual inference"
      "Internal"
      "Mutual.juvix",
    PosTest
      "open import a builtin multiple times"
      "BuiltinsMultiOpenImport"
      "Input.juvix",
    PosTest
      "As Patterns"
      "Internal"
      "AsPattern.juvix"
  ]
