module Core.Recursor.Base where

import Base
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty

data UnitTest = UnitTest
  { _name :: String,
    _function :: Node -> Node,
    _inputOutputPairs :: [(Node, Node)]
  }

makeLenses ''UnitTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests")

testDescr :: UnitTest -> TestDescr
testDescr test@UnitTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = root,
      _testAssertion = Steps $ unitTestAssertion test
    }

unitTestAssertion :: UnitTest -> (String -> IO ()) -> Assertion
unitTestAssertion UnitTest {..} _ =
  forM_ _inputOutputPairs $ \(i, o) -> do
    assertBool
      ("Unit test failed.\ngot:\n\t" <> fromText (ppTrace (_function i)) <> "\nexpected:\n\t" <> fromText (ppTrace o))
      (_function i == o)
