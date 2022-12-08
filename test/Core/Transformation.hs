module Core.Transformation where

import Base
import Core.Transformation.Identity qualified as Identity
import Core.Transformation.Lifting qualified as Lifting
import Core.Transformation.RemoveTypeArgs qualified as RemoveTypeArgs
import Core.Transformation.TopEtaExpand qualified as TopEtaExpand

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore transformations"
    [ Identity.allTests,
      TopEtaExpand.allTests,
      Lifting.allTests,
      RemoveTypeArgs.allTests
    ]
