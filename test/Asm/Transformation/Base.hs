module Asm.Transformation.Base where

import Asm.Run.Base
import Asm.Run.Positive qualified as Run
import Base
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error

data Test = Test
  { _testTransformation :: InfoTable -> Either AsmError InfoTable,
    _testAssertion :: InfoTable -> Assertion,
    _testEval :: Run.PosTest
  }

fromTest :: Test -> TestTree
fromTest = mkTest . toTestDescr

troot :: FilePath
troot = "tests/Asm/positive/"

toTestDescr :: Test -> TestDescr
toTestDescr Test {..} =
  let Run.PosTest {..} = _testEval
      tRoot = troot </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmRunAssertion _file _expectedFile _testTransformation _testAssertion
        }
