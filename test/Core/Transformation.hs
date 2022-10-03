module Core.Transformation where

import Base
import Core.Transformation.Lifting qualified as Lifting
import Core.Transformation.TopEtaExpand qualified as TopEtaExpand

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore transformations"
    [Lifting.allTests, TopEtaExpand.allTests]
