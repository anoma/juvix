module Scope.Positive where

import Base
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text qualified as M
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as M
import MiniJuvix.Syntax.Concrete.Scoped.Utils
import Text.Show.Pretty hiding (Html)

data PosTest = PosTest
  { name :: String,
    relDir :: FilePath,
    file :: FilePath
  }

root :: FilePath
root = "tests/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { testName = name,
      testRoot = root </> relDir,
      testAssertion = Steps $ \step -> do
        step "Parse"
        p <- parseModuleIO file

        step "Scope"
        s <- scopeModuleIO p
        let fs :: HashMap FilePath Text
            fs =
              HashMap.fromList
                [ (getModuleFilePath m, M.renderPrettyCodeDefault m)
                  | m <- toList (getAllModules s)
                ]

        step "Pretty"
        let scopedPretty = M.renderPrettyCodeDefault s
        let parsedPretty = M.renderPrettyCodeDefault p

        step "Parse again"
        p' <- parseTextModuleIO scopedPretty
        parsedPretty' <- parseTextModuleIO parsedPretty

        step "Scope again"
        s' <-
          head . Scoper._resultModules
            <$> fromRightIO' printErrorAnsi (return (Scoper.scopeCheck1Pure fs "." p'))
        step "Checks"
        assertEqDiff "check: scope . parse . pretty . scope . parse = scope . parse" s s'
        assertEqDiff "check: parse . pretty . scope . parse = parse" p p'
        assertEqDiff "check: parse . pretty . parse = parse" p parsedPretty'
    }

assertEqDiff :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqDiff msg a b
  | a == b = return ()
  | otherwise = do
      putStrLn (pack $ ppDiff (getGroupedDiff pa pb))
      putStrLn "End diff"
      fail msg
  where
    pa = lines $ ppShow a
    pb = lines $ ppShow b

allTests :: TestTree
allTests =
  testGroup
    "Scope positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Inductive"
      "."
      "Inductive.mjuvix",
    PosTest
      "Imports and qualified names"
      "Imports"
      "A.mjuvix",
    PosTest
      "Data.Bool from the stdlib"
      "StdlibList"
      "Data/Bool.mjuvix",
    PosTest
      "Data.Nat from the stdlib"
      "StdlibList"
      "Data/Nat.mjuvix",
    PosTest
      "Data.Ord from the stdlib"
      "StdlibList"
      "Data/Ord.mjuvix",
    PosTest
      "Data.Product from the stdlib"
      "StdlibList"
      "Data/Product.mjuvix",
    PosTest
      "Data.List and friends from the stdlib"
      "StdlibList"
      "Data/List.mjuvix",
    PosTest
      "Operators (+)"
      "."
      "Operators.mjuvix",
    PosTest
      "Literals"
      "."
      "Literals.mjuvix",
    PosTest
      "Hello World backends"
      "."
      "HelloWorld.mjuvix",
    PosTest
      "Axiom with backends"
      "."
      "Axiom.mjuvix",
    PosTest
      "Foreign block parsing"
      "."
      "Foreign.mjuvix",
    PosTest
      "Multiple modules non-ambiguous symbol - same file"
      "QualifiedSymbol"
      "M.mjuvix",
    PosTest
      "Multiple modules non-ambiguous symbol"
      "QualifiedSymbol2"
      "N.mjuvix",
    PosTest
      "Multiple modules constructor non-ambiguous symbol"
      "QualifiedConstructor"
      "M.mjuvix",
    PosTest
      "open overrides open public"
      "."
      "ShadowPublicOpen.mjuvix"
  ]
