module Scope.Positive where

import Base
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text as M


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
    -- do something

    step "Scope"
    -- do something
    s <- scopeModuleIO p

    step "Pretty"
    let txt = M.renderPrettyCodeDefault s

    step "Parse again"
    p' <- parseTextModuleIO txt
    assertEqual "check: parse. pretty . scope . parse = id" p p'

    step "Scope again"
    s' <- scopeModuleIO p
    assertEqual "check: scope . parse . pretty . scope . parse = id" s s'
  }

allTests :: TestTree
allTests = testGroup "Scope positive tests"
  (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests = [
  PosTest "Inductive" "." "Inductive.mjuvix"

 ]
