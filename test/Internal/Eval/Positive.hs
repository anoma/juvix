module Internal.Eval.Positive where

import Base
import Internal.Eval.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Internal/positive")

coreRoot :: Path Abs Dir
coreRoot = relToProject $(mkRelDir "tests/Internal/Core/positive")

testDescr :: Path Abs Dir -> PosTest -> TestDescr
testDescr r PosTest {..} =
  let tRoot = r <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ internalCoreAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "Internal to Core positive tests"
    (map (mkTest . testDescr root) tests ++ map (mkTest . testDescr coreRoot) coreTests)

coreTests :: [PosTest]
coreTests =
  [ PosTest
      "If then else"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Fibonacci"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "out/test011.out")
  ]

tests :: [PosTest]
tests =
  [ PosTest
      "An integer literal"
      $(mkRelDir ".")
      $(mkRelFile "IntegerLiteral.juvix")
      $(mkRelFile "out/IntegerLiteral.out"),
    PosTest
      "A zero argument function"
      $(mkRelDir ".")
      $(mkRelFile "IdenFunctionIntegerLiteral.juvix")
      $(mkRelFile "out/IdenFunctionIntegerLiteral.out"),
    PosTest
      "A two argument function"
      $(mkRelDir ".")
      $(mkRelFile "IdenFunctionArgs.juvix")
      $(mkRelFile "out/IdenFunctionArgs.out"),
    PosTest
      "A function with implicit arguments"
      $(mkRelDir ".")
      $(mkRelFile "IdenFunctionArgsImplicit.juvix")
      $(mkRelFile "out/IdenFunctionArgsImplicit.out"),
    PosTest
      "A module that imports another"
      $(mkRelDir "Import")
      $(mkRelFile "Importer.juvix")
      $(mkRelFile "out/Importer.out"),
    PosTest
      "A constructor valued function"
      $(mkRelDir ".")
      $(mkRelFile "FunctionReturnConstructor.juvix")
      $(mkRelFile "out/FunctionReturnConstructor.out"),
    PosTest
      "Pattern matching on a constructor"
      $(mkRelDir ".")
      $(mkRelFile "MatchConstructor.juvix")
      $(mkRelFile "out/MatchConstructor.out"),
    PosTest
      "Pattern matching Nat under suc"
      $(mkRelDir ".")
      $(mkRelFile "NatMatch1.juvix")
      $(mkRelFile "out/NatMatch1.out"),
    PosTest
      "Pattern matching Nat as binder"
      $(mkRelDir ".")
      $(mkRelFile "NatMatch2.juvix")
      $(mkRelFile "out/NatMatch2.out"),
    PosTest
      "Literal Nat"
      $(mkRelDir ".")
      $(mkRelFile "LitInteger.juvix")
      $(mkRelFile "out/LitInteger.out"),
    PosTest
      "Pattern matching on literal Nat"
      $(mkRelDir ".")
      $(mkRelFile "LitIntegerToNat.juvix")
      $(mkRelFile "out/LitIntegerToNat.out"),
    PosTest
      "Literal String"
      $(mkRelDir ".")
      $(mkRelFile "LitString.juvix")
      $(mkRelFile "out/LitString.out"),
    PosTest
      "Literal String IO"
      $(mkRelDir ".")
      $(mkRelFile "LitStringIO.juvix")
      $(mkRelFile "out/LitStringIO.out"),
    PosTest
      "Mutually defined functions"
      $(mkRelDir ".")
      $(mkRelFile "Mutual.juvix")
      $(mkRelFile "out/Mutual.out"),
    PosTest
      "Calling builtin addition"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinAdd.juvix")
      $(mkRelFile "out/BuiltinAdd.out"),
    PosTest
      "Builtin bool"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinBool.juvix")
      $(mkRelFile "out/BuiltinBool.out"),
    PosTest
      "Builtin if"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinIf.juvix")
      $(mkRelFile "out/BuiltinIf.out"),
    PosTest
      "Lambda"
      $(mkRelDir ".")
      $(mkRelFile "Lambda.juvix")
      $(mkRelFile "out/Lambda.out"),
    PosTest
      "Pattern args"
      $(mkRelDir ".")
      $(mkRelFile "PatternArgs.juvix")
      $(mkRelFile "out/PatternArgs.out"),
    PosTest
      "QuickSort"
      $(mkRelDir ".")
      $(mkRelFile "QuickSort.juvix")
      $(mkRelFile "out/QuickSort.out"),
    PosTest
      "Universe"
      $(mkRelDir ".")
      $(mkRelFile "Universe.juvix")
      $(mkRelFile "out/Universe.out"),
    PosTest
      "Inductive type constructor"
      $(mkRelDir ".")
      $(mkRelFile "Inductive.juvix")
      $(mkRelFile "out/Inductive.out"),
    PosTest
      "Function type"
      $(mkRelDir ".")
      $(mkRelFile "FunctionType.juvix")
      $(mkRelFile "out/FunctionType.out"),
    PosTest
      "Builtin Inductive type"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinInductive.juvix")
      $(mkRelFile "out/BuiltinInductive.out"),
    PosTest
      "Higher Order Lambda"
      $(mkRelDir ".")
      $(mkRelFile "HigherOrderLambda.juvix")
      $(mkRelFile "out/HigherOrderLambda.out"),
    PosTest
      "Type Aliases"
      $(mkRelDir ".")
      $(mkRelFile "Church.juvix")
      $(mkRelFile "out/Church.out"),
    PosTest
      "Nested as patterns"
      $(mkRelDir ".")
      $(mkRelFile "AsPatterns.juvix")
      $(mkRelFile "out/AsPatterns.out")
  ]
