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
    _testAssertion :: InfoTable -> Assertion,
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

assertExpectedOutput :: FilePath -> InfoTable -> Assertion
assertExpectedOutput testExpectedFile r = do
  expected <- readFile testExpectedFile
  let actualOutput = Text.renderStrict (toTextStream (ppOut opts r))
  assertEqDiff ("Check: output = " <> testExpectedFile) actualOutput expected
  where
    opts :: Options
    opts =
      defaultOptions
        { _optShowDeBruijnIndices = True
        }

coreTransAssertion :: Test -> Assertion
coreTransAssertion Test {..} = do
  r <- applyTransformations [LambdaLifting] <$> parseFile _testCoreFile
  _testAssertion r

parseFile :: FilePath -> IO InfoTable
parseFile f = fst <$> fromRightIO show (runParser "" f emptyInfoTable <$> readFile f)
