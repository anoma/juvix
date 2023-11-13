module Typecheck.PositiveNew where

import Base
import Data.HashSet qualified as HashSet
import Typecheck.Positive qualified as Old

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> Old.PosTest
posTest _name rdir rfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
   in Old.PosTest {..}

testDescr :: Old.PosTest -> TestDescr
testDescr Old.PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Single $ do
        entryPoint <- set entryPointNewTypeCheckingAlgorithm True <$> defaultEntryPointCwdIO _file
        (void . runIO' entryPoint) upToInternalTyped
    }

allTests :: TestTree
allTests =
  testGroup
    "New typechecker positive tests"
    [ testGroup
        "New typechecker General typechecking tests"
        (map (mkTest . testDescr) (filter (not . isIgnored) (extraTests <> Old.tests)))
    ]

isIgnored :: Old.PosTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

extraTests :: [Old.PosTest]
extraTests =
  [ Old.posTest
      "Monad transformers (State)"
      $(mkRelDir "Monads")
      $(mkRelFile "State.juvix"),
    Old.posTest
      "Monad transformers (Reader)"
      $(mkRelDir "Monads")
      $(mkRelFile "Reader.juvix"),
    Old.posTest
      "Monad transformers (ReaderT)"
      $(mkRelDir "Monads")
      $(mkRelFile "ReaderT.juvix")
  ]

-- | Default values are not supported by the new type checker at the moment
ignored :: HashSet String
ignored =
  HashSet.fromList
    [ "Test066: Import function with a function call in default argument",
      "Test068: Dependent default values inserted in the arity checker",
      "Test069: Dependent default values for Ord trait",
      "Test070: Nested default values and named arguments",
      "Test071: Named application",
      -- This test does not pass with the new hole insertion algorithm
      "Test046: Polymorphic type arguments"
    ]
