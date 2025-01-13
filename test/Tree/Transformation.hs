module Tree.Transformation where

import Base
import Tree.Transformation.Apply qualified as Apply
import Tree.Transformation.CheckNoAnoma qualified as CheckNoAnoma
import Tree.Transformation.ConvertUnaryCalls qualified as ConvertUnaryCalls
import Tree.Transformation.IdentityTrans qualified as IdentityTrans
import Tree.Transformation.Reachability qualified as Reachability

allTests :: TestTree
allTests =
  testGroup
    "JuvixTree transformations"
    [ IdentityTrans.allTests,
      Apply.allTests,
      ConvertUnaryCalls.allTests,
      Reachability.allTests,
      CheckNoAnoma.allTests
    ]
