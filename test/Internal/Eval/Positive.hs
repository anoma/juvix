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
      "Zero is the constant 0"
      "."
      "FunctionReturnZero.juvix"
      "out/FunctionReturnZero.out",
    PosTest
      "Suc returns a constant integer"
      "."
      "FunctionReturnSuc.juvix"
      "out/FunctionReturnSuc.out"
  ]
