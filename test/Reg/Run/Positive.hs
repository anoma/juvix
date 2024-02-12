module Reg.Run.Positive where

import Base
import Reg.Parse.Positive qualified as Parse
import Reg.Run.Base

type PosTest = Parse.PosTest

testDescr :: PosTest -> TestDescr
testDescr Parse.PosTest {..} =
  let tRoot = Parse.root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ regRunAssertion file' expected' [] (const (return ()))
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixReg parsing positive tests"
    (map (mkTest . testDescr) Parse.tests)
