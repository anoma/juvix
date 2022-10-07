module Core.Transformation where

import Base
import Core.Transformation.Identity qualified as Identity
import Core.Transformation.Lifting qualified as Lifting

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore transformations"
    [
      Identity.allTests,
      Lifting.allTests
    ]
