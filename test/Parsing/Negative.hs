module Parsing.Negative where

import Base
import Juvix.Compiler.Pipeline
import Juvix.Parser.Error

root :: FilePath
root = "tests/negative"

data NegTest = NegTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint _file
            res <- runIOEither (upToParsing entryPoint)
            case mapLeft fromJuvixError res of
              Left (Just (_ :: ParserError)) -> return ()
              Left Nothing -> assertFailure "The parser did not find an error."
              Right _ -> assertFailure "An error ocurred but it was not in the parser."
        }

allTests :: TestTree
allTests =
  testGroup
    "Parsing negative tests"
    ( map (mkTest . testDescr) scoperErrorTests
    )

scoperErrorTests :: [NegTest]
scoperErrorTests =
  [ NegTest
      "Tab character"
      "."
      "Tab.juvix"
  ]
