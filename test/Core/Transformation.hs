module Core.Transformation where

import Base
import Core.Transformation.Lifting qualified as Lifting

allTests :: TestTree
allTests = testGroup "JuvixCore transformations" [Lifting.allTests]
