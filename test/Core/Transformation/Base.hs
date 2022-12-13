module Core.Transformation.Base where

import Base
import Core.Eval.Base
import Core.Eval.Positive qualified as Eval
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Transformation

data Test = Test
  { _testTransformations :: [TransformationId],
    _testAssertion :: InfoTable -> Assertion,
    _testEval :: Eval.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Core/positive/")

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Eval.PosTest {..} = _testEval
      tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalAssertion file' expected' _testTransformations _testAssertion
        }
