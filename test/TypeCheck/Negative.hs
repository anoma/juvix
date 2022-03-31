module TypeCheck.Negative (allTests) where

import Base
import MiniJuvix.Syntax.MicroJuvix.Error
import qualified MiniJuvix.Syntax.MicroJuvix.TypeChecker as T
import qualified MiniJuvix.Translation.AbstractToMicroJuvix as A

type FailMsg = String

data NegTest = NegTest
  { name :: String,
    relDir :: FilePath,
    file :: FilePath,
    checkErr :: TypeCheckerError -> Maybe FailMsg }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} = TestDescr {
  testName = name,
  testRoot = root </> relDir,
  testAssertion = Single $ do
      result <- parseModuleIO file
                >>= scopeModuleIO
                >>= translateModuleIO
                >>| A.translateModule
                >>| T.checkModule

      case result of
        Left es -> when (all (/= Nothing) (checkErr <$> es))
                        (assertFailure "expected error not found")
        Right _ -> assertFailure "The type checker did not find an error."
  }

allTests :: TestTree
allTests = testGroup "TypeCheck negative tests"
  (map (mkTest . testDescr) tests)

root :: FilePath
root = "tests/negative"

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests = [
  NegTest "Constructor in pattern type error" "MicroJuvix"
    "PatternConstructor.mjuvix" $ \case
      ErrWrongConstructorType {} -> Nothing
      _ -> wrongError
  , NegTest "Constructor pattern length mismatch" "MicroJuvix"
    "PatternConstructorApp.mjuvix" $ \case
      ErrWrongConstructorAppArgs {} -> Nothing
      _ -> wrongError
  , NegTest "Type vs inferred type mismatch" "MicroJuvix"
    "WrongType.mjuvix" $ \case
      ErrWrongType {} -> Nothing
      _ -> wrongError
  , NegTest "Function application with non-function type" "MicroJuvix"
    "ExpectedFunctionType.mjuvix" $ \case
      ErrExpectedFunctionType {} -> Nothing
      _ -> wrongError
  , NegTest "Function definition clause with two many match patterns" "MicroJuvix"
    "TooManyPatterns.mjuvix" $ \case
      ErrTooManyPatterns {} -> Nothing
      _ -> wrongError
        ]
