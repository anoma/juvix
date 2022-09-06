module Core.Transformation.Base where

import Base
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Core.Translation.FromSource
import Prettyprinter.Render.Text qualified as Text

data Test = Test
  { _testName :: String,
    _testCoreFile :: FilePath,
    _testExpectedFile :: FilePath,
    _testTransformations :: [TransformationId]
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

troot :: FilePath
troot = "tests/Core/positive/"

toTestDescr :: Test -> TestDescr
toTestDescr t@Test {..} =
  TestDescr
    { _testName,
      _testRoot = troot,
      _testAssertion = Single (coreTransAssertion t)
    }

coreTransAssertion :: Test -> Assertion
coreTransAssertion Test {..} = do
  r <- applyTransformations [LambdaLifting] <$> parseFile _testCoreFile
  expected <- readFile _testExpectedFile
  let actualOutput = Text.renderStrict (toTextStream (ppOutDefault r))
  assertEqDiff ("Check: EVAL output = " <> _testExpectedFile) actualOutput expected

parseFile :: FilePath -> IO InfoTable
parseFile f = fst <$> fromRightIO show (runParser "" f emptyInfoTable <$> readFile f)
