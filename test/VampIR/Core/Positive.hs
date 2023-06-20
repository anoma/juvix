module VampIR.Core.Positive where

import Base
import Core.Normalize.Positive (PosTest (..))
import Core.Normalize.Positive qualified as Normalize
import VampIR.Core.Base

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

toTestDescr :: PosTest -> TestDescr
toTestDescr = Normalize.toTestDescr' vampirAssertion

allTests :: TestTree
allTests =
  testGroup
    "Core to VampIR translation positive tests"
    ( map
        (mkTest . toTestDescr)
        ( tests
            ++ ( Normalize.filterOutTests
                   ( -- VampIR stack overflow
                     [ "Test020: functional queues",
                       "Test026: letrec"
                     ]
                       ++
                       -- recursion takes too long
                       [ "Test014: recursion",
                         "Test015: tail recursion",
                         "Test016: tail recursion: Fibonacci numbers in linear time",
                         "Test017: recursion through higher-order functions",
                         "Test018: tail recursion through higher-order functions",
                         "Test022: mutual recursion"
                       ]
                   )
                   Normalize.tests
               )
        )
    )

tests :: [PosTest]
tests =
  [ PosTest
      5
      "Test001"
      $(mkRelDir "translation")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      5
      "Test002"
      $(mkRelDir "translation")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json")
  ]
