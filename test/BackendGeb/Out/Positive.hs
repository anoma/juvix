module BackendGeb.Out.Positive where

import Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize
import BackendGeb.Out.Base

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' (gebAssertion VampirHalo2)

toPlonkTestDescr :: PosTest -> TestDescr
toPlonkTestDescr = Normalize.toTestDescr' (gebAssertion VampirPlonk)

allTests :: TestTree
allTests =
  testGroup
    "VampIR translation through Geb positive tests"
    (map (mkTest . toTestDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001"
      $(mkRelDir "Core")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      "Test002"
      $(mkRelDir "Core")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json"),
    PosTest
      "Test003"
      $(mkRelDir "Core")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "data/test003.json"),
    PosTest
      "Test004"
      $(mkRelDir "Core")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "data/test004.json")
  ]
