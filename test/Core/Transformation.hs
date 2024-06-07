module Core.Transformation where

import Base
import Core.Transformation.IdentityTrans qualified as IdentityTrans
import Core.Transformation.Lifting qualified as Lifting
import Core.Transformation.Pipeline qualified as Pipeline
import Core.Transformation.TopEtaExpand qualified as TopEtaExpand
import Core.Transformation.Unrolling qualified as Unrolling

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore transformations"
    [ IdentityTrans.allTests,
      TopEtaExpand.allTests,
      Lifting.allTests,
      Pipeline.allTests,
      Unrolling.allTests
    ]
