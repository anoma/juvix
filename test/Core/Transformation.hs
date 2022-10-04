module Core.Transformation where

import Base
import Core.Transformation.Identity qualified as Identity
import Core.Transformation.Lifting qualified as Lifting
import Core.Transformation.TopEtaExpand qualified as TopEtaExpand

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore transformations"
    [ Lifting.allTests,
      TopEtaExpand.allTests,
      Identity.allTests
    ]
