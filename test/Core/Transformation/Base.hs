module Core.Transformation.Base where

import Base
import Core.Eval.Base
import Core.Eval.Positive qualified as Eval
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation
import Prettyprinter.Render.Text qualified as Text

data Test = Test
  { _testTransformations :: [TransformationId],
    _testAssertion :: InfoTable -> Assertion,
    _testEval :: Eval.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

troot :: FilePath
troot = "tests/Core/positive/"

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Eval.PosTest {..} = _testEval
      tRoot = troot </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalAssertion _file _expectedFile _testTransformations _testAssertion
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
