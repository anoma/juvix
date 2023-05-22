module Core.VampIR.LetHoist where

import Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize
import Core.VampIR.Base
import Juvix.Compiler.Core.Data.TransformationId

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' (const (coreVampIRAssertion [LetHoisting]))

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore VampIR let-hoist positive tests"
    (map (mkTest . toTestDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      5
      "Test010: Lets"
      $(mkRelDir "Core")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "data/test010.json"),
    PosTest
      5
      "Test028: Let hoisting"
      $(mkRelDir "Core")
      $(mkRelFile "test028.jvc")
      $(mkRelFile "data/test028.json"),
    PosTest
      5
      "Test029: Let hoisting"
      $(mkRelDir "Core")
      $(mkRelFile "test029.jvc")
      $(mkRelFile "data/test029.json")
  ]
