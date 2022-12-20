module Parsing.Negative where

import Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Pipeline
import Juvix.Parser.Error

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            let entryPoint = defaultEntryPoint tRoot file'
            res <- runIOEither iniState entryPoint upToParsing
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
      $(mkRelDir ".")
      $(mkRelFile "Tab.juvix")
  ]
