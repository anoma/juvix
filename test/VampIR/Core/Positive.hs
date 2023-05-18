module VampIR.Core.Positive where

import Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize
import VampIR.Core.Base

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' vampirAssertion

allTests :: TestTree
allTests =
  testGroup
    "VampIR translation positive tests"
    ( map
        (mkTest . toTestDescr)
        ( tests
            ++ ( -- filtering due to stack overflow in VampIR with too many definitions
                 Normalize.filterOutTests
                   [ "Test020: functional queues",
                     "Test026: letrec"
                   ]
                   Normalize.tests
               )
        )
    )

tests :: [PosTest]
tests =
  [ PosTest
      5
      "Test001"
      $(mkRelDir "translation")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      5
      "Test002"
      $(mkRelDir "translation")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json")
  ]
