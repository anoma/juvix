module Compilation.PositiveNew where

import Base
import Compilation.Base
import Compilation.Positive qualified as Old
import Data.HashSet qualified as HashSet

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> Old.PosTest
posTest = posTest' EvalAndCompile

posTest' :: CompileAssertionMode -> String -> Path Rel Dir -> Path Rel File -> Path Rel File -> Old.PosTest
posTest' _assertionMode _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = root <//> routfile
   in Old.PosTest {..}

testDescr :: Int -> Old.PosTest -> TestDescr
testDescr optLevel Old.PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion =
        Steps $
        let f = set entryPointNewTypeCheckingAlgorithm True
        in compileAssertionEntry f _dir optLevel _assertionMode _file _expectedFile
    }

allTestsNoOptimize :: TestTree
allTestsNoOptimize =
  testGroup
    "New typechecker compilation positive tests (no optimization)"
    (map (mkTest . testDescr 0) (filter (not . isIgnored) (extraTests <> Old.tests)))

isIgnored :: Old.PosTest -> Bool
isIgnored t = HashSet.member (t ^. Old.name) ignored

extraTests :: [Old.PosTest]
extraTests =
  [ Old.posTest
      "Test073: Monad transformers (ReaderT + StateT + Identity)"
      $(mkRelDir "test072")
      $(mkRelFile "ReaderT.juvix")
      $(mkRelFile "out/test072.out")
  ]

ignored :: HashSet String
ignored =
  HashSet.fromList
    ["Test070: Nested default values and named arguments",
     "Test071: Named application (Ord instance with default cmp)"
    ]
