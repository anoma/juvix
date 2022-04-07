module TypeCheck.Positive where

import Base
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.TypeChecker qualified as T
import MiniJuvix.Translation.AbstractToMicroJuvix qualified as A

data PosTest = PosTest
  { name :: String,
    relDir :: FilePath,
    file :: FilePath
  }

root :: FilePath
root = "tests/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { testName = name,
      testRoot = root </> relDir,
      testAssertion = Single $ do
        result <-
          parseModuleIO file
            >>= scopeModuleIO
            >>= translateModuleIO
            >>| A.translateModule
            >>| T.checkModule

        case result of
          Left es -> assertFailure ("The type checker returned the errors:\n" <> show (ppTypeCheckerError <$> toList es))
          Right _ -> return ()
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
      "GHC backend SimpleFungibleToken"
      "VP"
      "SimpleFungibleToken.mjuvix",
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
      "Operators.mjuvix"
  ]
