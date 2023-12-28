module Nockma.Compile.Positive where

import Base
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromAsm

data Test = Test
  { _testName :: Text,
    _testExpectedResult :: Term Natural,
    _testProgram :: NockmaCompiler '[Embed IO]
  }

makeLenses ''Test

newtype PPTerm = PPTerm (Term Natural)

allTests :: TestTree
allTests = testGroup "Nockma compile unit positive" (map mk tests)
  where
    mk :: Test -> TestTree
    mk Test {..} = testCase (unpack _testName) $ do
      n <- debugProg _testProgram
      unless (n == _testExpectedResult) (err n)
      where
        err :: Term Natural -> IO ()
        err n = do
          let msg =
                "Expected:\n"
                  <> ppTrace _testExpectedResult
                  <> "But got:\n"
                  <> ppTrace n
          assertFailure (unpack msg)

tests :: [Test]
tests = undefined
