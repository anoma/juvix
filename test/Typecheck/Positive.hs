module Typecheck.Positive where

import Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Pipeline
import Typecheck.Negative qualified as N

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint tRoot file'
            (void . runIO' iniState entryPoint) upToInternalTyped
        }

--------------------------------------------------------------------------------
-- Testing --no-positivity flag with all related negative tests
--------------------------------------------------------------------------------

rootNegTests :: Path Abs Dir
rootNegTests = relToProject $(mkRelDir "tests/negative/")

testNoPositivityFlag :: N.NegTest -> TestDescr
testNoPositivityFlag N.NegTest {..} =
  let tRoot = rootNegTests <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint =
                  (defaultEntryPoint tRoot file')
                    { _entryPointNoPositivity = True
                    }

            (void . runIO' iniState entryPoint) upToInternal
        }

negPositivityTests :: [N.NegTest]
negPositivityTests = N.negPositivityTests

testPositivityKeyword :: [PosTest]
testPositivityKeyword =
  [ PosTest
      "Mark T0 data type as strictly positive"
      $(mkRelDir "Internal/Positivity")
      $(mkRelFile "E5.juvix")
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
      $(mkRelDir "Internal")
      $(mkRelFile "Simple.juvix"),
    PosTest
      "Literal String matches any type"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralString.juvix"),
    PosTest
      "Box type"
      $(mkRelDir "Internal")
      $(mkRelFile "Box.juvix"),
    PosTest
      "Literal Int matches any type"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralInt.juvix"),
    PosTest
      "PolySimpleFungibleToken"
      $(mkRelDir "FullExamples")
      $(mkRelFile "SimpleFungibleTokenImplicit.juvix"),
    PosTest
      "GHC backend MonoSimpleFungibleToken"
      $(mkRelDir "FullExamples")
      $(mkRelFile "MonoSimpleFungibleToken.juvix"),
    PosTest
      "Axiom"
      $(mkRelDir ".")
      $(mkRelFile "Axiom.juvix"),
    PosTest
      "Inductive"
      $(mkRelDir ".")
      $(mkRelFile "Inductive.juvix"),
    PosTest
      "Operators"
      $(mkRelDir ".")
      $(mkRelFile "Operators.juvix"),
    PosTest
      "Holes in type signature"
      $(mkRelDir "Internal")
      $(mkRelFile "HoleInSignature.juvix"),
    PosTest
      "Polymorphism and higher rank functions"
      $(mkRelDir ".")
      $(mkRelFile "Polymorphism.juvix"),
    PosTest
      "Polymorphism and higher rank functions with explicit holes"
      $(mkRelDir ".")
      $(mkRelFile "PolymorphismHoles.juvix"),
    PosTest
      "Implicit arguments"
      $(mkRelDir "Internal")
      $(mkRelFile "Implicit.juvix"),
    PosTest
      "Simple type alias"
      $(mkRelDir ".")
      $(mkRelFile "TypeAlias.juvix"),
    PosTest
      "Refine hole in type signature"
      $(mkRelDir "272")
      $(mkRelFile "M.juvix"),
    PosTest
      "Pattern match a hole type"
      $(mkRelDir "265")
      $(mkRelFile "M.juvix"),
    PosTest
      "Pattern match type synonym"
      $(mkRelDir "issue1466")
      $(mkRelFile "M.juvix"),
    PosTest
      "Import a builtin multiple times"
      $(mkRelDir "BuiltinsMultiImport")
      $(mkRelFile "Input.juvix"),
    PosTest
      "Basic lambda functions"
      $(mkRelDir "Internal")
      $(mkRelFile "Lambda.juvix"),
    PosTest
      "Simple mutual inference"
      $(mkRelDir "Internal")
      $(mkRelFile "Mutual.juvix"),
    PosTest
      "open import a builtin multiple times"
      $(mkRelDir "BuiltinsMultiOpenImport")
      $(mkRelFile "Input.juvix"),
    PosTest
      "As Patterns"
      $(mkRelDir "Internal")
      $(mkRelFile "AsPattern.juvix")
  ]
