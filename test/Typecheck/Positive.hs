module Typecheck.Positive where

import Base
import Compilation.Positive qualified as Compilation
import Typecheck.Negative qualified as N

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> PosTest
posTest _name rdir rfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
   in PosTest {..}

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Single $ do
        entryPoint <- defaultEntryPointCwdIO _file
        (void . runIO' entryPoint) upToInternalTyped
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
            entryPoint <-
              set entryPointNoPositivity True
                <$> defaultEntryPointCwdIO file'
            (void . runIO' entryPoint) upToInternal
        }

negPositivityTests :: [N.NegTest]
negPositivityTests = N.negPositivityTests

testWellDefinedInductiveDefs :: [PosTest]
testWellDefinedInductiveDefs =
  [ posTest
      "Rose tree definition is well-defined"
      $(mkRelDir "Internal/Positivity")
      $(mkRelFile "RoseTree.juvix")
  ]

testPositivityKeyword :: [PosTest]
testPositivityKeyword =
  [ posTest
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
        (map (mkTest . testDescr) testPositivityKeyword),
      testGroup
        "Well-defined inductive definitions"
        (map (mkTest . testDescr) testWellDefinedInductiveDefs)
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

compilationTest :: Compilation.PosTest -> PosTest
compilationTest Compilation.PosTest {..} = PosTest {..}

tests :: [PosTest]
tests =
  [ posTest
      "Simple"
      $(mkRelDir "Internal")
      $(mkRelFile "Simple.juvix"),
    posTest
      "Literal String matches any type"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralString.juvix"),
    posTest
      "Box type"
      $(mkRelDir "Internal")
      $(mkRelFile "Box.juvix"),
    posTest
      "Literal Int matches any type"
      $(mkRelDir "Internal")
      $(mkRelFile "LiteralInt.juvix"),
    posTest
      "Axiom"
      $(mkRelDir ".")
      $(mkRelFile "Axiom.juvix"),
    posTest
      "Inductive"
      $(mkRelDir ".")
      $(mkRelFile "Inductive.juvix"),
    posTest
      "Operators"
      $(mkRelDir ".")
      $(mkRelFile "Operators.juvix"),
    posTest
      "Holes in type signature"
      $(mkRelDir "Internal")
      $(mkRelFile "HoleInSignature.juvix"),
    posTest
      "Polymorphism and higher rank functions"
      $(mkRelDir ".")
      $(mkRelFile "Polymorphism.juvix"),
    posTest
      "Polymorphism and higher rank functions with explicit holes"
      $(mkRelDir ".")
      $(mkRelFile "PolymorphismHoles.juvix"),
    posTest
      "Implicit arguments"
      $(mkRelDir "Internal")
      $(mkRelFile "Implicit.juvix"),
    posTest
      "Simple type alias"
      $(mkRelDir ".")
      $(mkRelFile "TypeAlias.juvix"),
    posTest
      "Refine hole in type signature"
      $(mkRelDir "272")
      $(mkRelFile "M.juvix"),
    posTest
      "Pattern match a hole type"
      $(mkRelDir "265")
      $(mkRelFile "M.juvix"),
    posTest
      "Pattern match type synonym"
      $(mkRelDir "issue1466")
      $(mkRelFile "M.juvix"),
    posTest
      "Import a builtin multiple times"
      $(mkRelDir "BuiltinsMultiImport")
      $(mkRelFile "Input.juvix"),
    posTest
      "Basic lambda functions"
      $(mkRelDir "Internal")
      $(mkRelFile "Lambda.juvix"),
    posTest
      "Simple mutual inference"
      $(mkRelDir "Internal")
      $(mkRelFile "Mutual.juvix"),
    posTest
      "open import a builtin multiple times"
      $(mkRelDir "BuiltinsMultiOpenImport")
      $(mkRelFile "Input.juvix"),
    posTest
      "As Patterns"
      $(mkRelDir "Internal")
      $(mkRelFile "AsPattern.juvix"),
    posTest
      "Issue 1693 (Inference and higher order functions)"
      $(mkRelDir "issue1693")
      $(mkRelFile "M.juvix"),
    posTest
      "Issue 1704 (Type synonyms)"
      $(mkRelDir "Internal")
      $(mkRelFile "Synonyms.juvix"),
    posTest
      "Issue 1731 Trace builtin for debugging"
      $(mkRelDir "issue1731")
      $(mkRelFile "builtinTrace.juvix"),
    posTest
      "Issue 1731 Fail builtin for debugging"
      $(mkRelDir "issue1731")
      $(mkRelFile "builtinFail.juvix"),
    posTest
      "Case expressions"
      $(mkRelDir "Internal")
      $(mkRelFile "Case.juvix"),
    posTest
      "Let shadowing"
      $(mkRelDir ".")
      $(mkRelFile "LetShadow.juvix"),
    posTest
      "Type synonym inside let"
      $(mkRelDir "issue1879")
      $(mkRelFile "LetSynonym.juvix"),
    posTest
      "Mutually recursive of lambda calculus with synonyms"
      $(mkRelDir ".")
      $(mkRelFile "LambdaCalculus.juvix"),
    posTest
      "Mutually recursive type synonym and inductive type"
      $(mkRelDir ".")
      $(mkRelFile "MutualType.juvix"),
    posTest
      "Type synonym inside local module"
      $(mkRelDir ".")
      $(mkRelFile "LocalSynonym.juvix"),
    posTest
      "Mutual inference inside let"
      $(mkRelDir ".")
      $(mkRelFile "MutualLet.juvix"),
    posTest
      "import inside local module"
      $(mkRelDir "issue2163")
      $(mkRelFile "Main.juvix"),
    posTest
      "id application in type"
      $(mkRelDir ".")
      $(mkRelFile "IdInType.juvix"),
    posTest
      "Nested pattern match with type variables"
      $(mkRelDir ".")
      $(mkRelFile "NestedPatterns.juvix"),
    posTest
      "issue2248: Import type alias"
      $(mkRelDir "issue2248")
      $(mkRelFile "Main.juvix"),
    posTest
      "Named arguments"
      $(mkRelDir ".")
      $(mkRelFile "NamedArguments.juvix"),
    posTest
      "Record declaration"
      $(mkRelDir ".")
      $(mkRelFile "Records.juvix")
  ]
    <> [ compilationTest t | t <- Compilation.tests
       ]
