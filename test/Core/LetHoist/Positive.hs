module Core.LetHoist.Positive where

import Base
import Core.LetHoist.Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' coreLetHoistAssertion

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore let-hoist positive tests"
    (map (mkTest . toTestDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test010: Lets"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "data/test010.json"),
    PosTest
      "Test028: Lets"
      $(mkRelDir ".")
      $(mkRelFile "test028.jvc")
      $(mkRelFile "data/test028.json"),
    PosTest
      "Test029: Lets"
      $(mkRelDir ".")
      $(mkRelFile "test029.jvc")
      $(mkRelFile "data/test029.json")
  ]
