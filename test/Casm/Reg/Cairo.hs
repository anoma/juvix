module Casm.Reg.Cairo where

import Base
import Casm.Reg.Base
import Casm.Reg.Positive qualified as P

testDescr :: P.PosTest -> TestDescr
testDescr P.PosTest {..} =
  let tRoot = P.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regToCairoAssertion file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg to Cairo translation positive tests"
    ( map (mkTest . testDescr) $
        P.filterOutTests
          []
          P.tests
    )
