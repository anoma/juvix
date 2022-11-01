module Internal.Eval.Positive where

import Base
import Internal.Eval.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _expectedFile :: FilePath
  }

root :: FilePath
root = "tests/Internal/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ internalCoreAssertion _file _expectedFile
        }

allTests :: TestTree
allTests =
  testGroup
    "Internal to Core positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "An integer literal"
      "."
      "IntegerLiteral.juvix"
      "out/IntegerLiteral.out",
    PosTest
      "A zero argument function"
      "."
      "IdenFunctionIntegerLiteral.juvix"
      "out/IdenFunctionIntegerLiteral.out",
    PosTest
      "A two argument function"
      "."
      "IdenFunctionArgs.juvix"
      "out/IdenFunctionArgs.out",
    PosTest
      "A function with implicit arguments"
      "."
      "IdenFunctionArgsImplicit.juvix"
      "out/IdenFunctionArgsImplicit.out",
    PosTest
      "A function with no explicit arguments"
      "."
      "IdenFunctionArgsNoExplicit.juvix"
      "out/IdenFunctionArgsNoExplicit.out",
    PosTest
      "A module that imports another"
      "Import"
      "Importer.juvix"
      "out/Importer.out",
    PosTest
      "A constructor valued function"
      "."
      "FunctionReturnConstructor.juvix"
      "out/FunctionReturnConstructor.out",
    PosTest
      "Pattern matching on a constructor"
      "."
      "MatchConstructor.juvix"
      "out/MatchConstructor.out",
    PosTest
      "Pattern matching Nat under suc"
      "."
      "NatMatch1.juvix"
      "out/NatMatch1.out",
    PosTest
      "Pattern matching Nat as binder"
      "."
      "NatMatch2.juvix"
      "out/NatMatch2.out",
    PosTest
      "Literal integer is Core integer"
      "."
      "LitInteger.juvix"
      "out/LitInteger.out",
    PosTest
      "Literal integer is Core string"
      "."
      "LitString.juvix"
      "out/LitString.out",
    PosTest
      "Mutually defined functions"
      "."
      "Mutual.juvix"
      "out/Mutual.out",
    PosTest
      "Calling builtin addition"
      "."
      "BuiltinAdd.juvix"
      "out/BuiltinAdd.out",
    PosTest
      "Builtin bool"
      "."
      "BuiltinBool.juvix"
      "out/BuiltinBool.out",
    PosTest
      "Builtin if"
      "."
      "BuiltinIf.juvix"
      "out/BuiltinIf.out",
    PosTest
      "Lambda"
      "."
      "Lambda.juvix"
      "out/Lambda.out"
  ]
