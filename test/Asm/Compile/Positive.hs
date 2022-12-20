module Asm.Compile.Positive where

import Asm.Compile.Base
import Asm.Run.Positive qualified as Run
import Base

testDescr :: Run.PosTest -> TestDescr
testDescr Run.PosTest {..} =
  let tRoot = Run.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ asmCompileAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixAsm compile positive tests"
    (map (mkTest . testDescr) Run.tests)
