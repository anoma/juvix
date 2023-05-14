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
tests = Normalize.tests
