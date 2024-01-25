module Tree.Transformation where

import Base
import Tree.Transformation.Apply qualified as Apply
import Tree.Transformation.Identity qualified as Identity

allTests :: TestTree
allTests =
  testGroup
    "JuvixTree transformations"
    [ Identity.allTests,
      Apply.allTests
    ]
