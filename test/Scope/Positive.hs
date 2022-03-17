module Scope.Positive where

import Base
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text as M
import qualified MiniJuvix.Syntax.Concrete.Scoped.Scoper as M
import MiniJuvix.Syntax.Concrete.Scoped.Utils
import qualified Data.HashMap.Strict as HashMap


data PosTest = PosTest {
  name :: String,
  relDir :: FilePath,
  file :: FilePath
  }

root :: FilePath
root = "tests/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} = TestDescr {
  testName = name,
  testRoot = root </> relDir,
  testAssertion = Steps $ \step -> do
    step "Parse"
    p <- parseModuleIO file

    step "Scope"
    s <- scopeModuleIO p
    let
      fs :: HashMap FilePath Text
      fs = HashMap.fromList
         [ (getModuleFilePath m , M.renderPrettyCodeDefault m)
           | m <- toList (getAllModules s) ]

    step "Pretty"
    let scopedPretty = M.renderPrettyCodeDefault s
    let parsedPretty = M.renderPrettyCodeDefault p

    step "Parse again"
    p' <- parseTextModuleIO scopedPretty
    parsedPretty' <- parseTextModuleIO parsedPretty

    step "Scope again"
    s' <- fromRightIO' printErrorAnsi $ M.scopeCheck1Pure fs "." p'

    step "Checks"
    assertBool "check: scope . parse . pretty . scope . parse = scope . parse" (s == s')
    assertBool "check: parse . pretty . scope . parse = parse" (p == p')
    assertBool "check: parse . pretty . parse = parse" (p == parsedPretty')
  }

allTests :: TestTree
allTests = testGroup "Scope positive tests"
  (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests = [
  PosTest "Inductive"
     "." "Inductive.mjuvix"
  , PosTest "Imports and qualified names"
     "Imports" "A.mjuvix"
  , PosTest "Data.Bool from the stdlib"
     "StdlibList" "Data/Bool.mjuvix"
  , PosTest "Data.Nat from the stdlib"
     "StdlibList" "Data/Nat.mjuvix"
  , PosTest "Data.Ord from the stdlib"
     "StdlibList" "Data/Ord.mjuvix"
  , PosTest "Data.Product from the stdlib"
     "StdlibList" "Data/Product.mjuvix"
  , PosTest "Data.List and friends from the stdlib"
     "StdlibList" "Data/List.mjuvix"
  , PosTest "Operators (+)"
     "." "Operators.mjuvix"
  , PosTest "Literals"
     "." "Literals.mjuvix"
 ]
