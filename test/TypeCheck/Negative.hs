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
      p <- parseModuleIO file
           >>= scopeModuleIO
           >>= translateModuleIO
           >>| A.translateModule
           >>| T.checkModule

      case p of
        Left err -> whenJust (checkErr err) assertFailure
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
        ]
