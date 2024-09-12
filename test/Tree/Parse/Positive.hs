module Tree.Parse.Positive where

import Base
import Tree.Eval.Positive qualified as Eval
import Tree.Parse.Base

type PosTest = Eval.PosTest

testDescr :: PosTest -> TestDescr
testDescr Eval.PosTest {..} =
  let tRoot = Eval.root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ treeParseAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixTree parsing positive tests"
    (map (mkTest . testDescr) Eval.tests)
