module BackendMarkdown.Negative where

import Base
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Parser.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: ParserError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- defaultEntryPointCwdIO file'
            result <- runIOEither entryPoint upToParsing
            case mapLeft fromJuvixError result of
              Left (Just err) -> whenJust (_checkErr err) assertFailure
              Right _ -> assertFailure "Unexpected success."
              Left Nothing -> assertFailure "Unexpected error."
        }

allTests :: TestTree
allTests =
  testGroup
    "Markdown negative tests"
    (map (mkTest . testDescr) tests)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

wrongError :: Maybe FailMsg
wrongError = Just "Incorrect error"

tests :: [NegTest]
tests =
  [ NegTest
      "Empty file"
      $(mkRelDir "Markdown")
      $(mkRelFile "Empty.juvix.md")
      $ \case
        ErrMarkdownBackend (ErrNoJuvixCodeBlocks _) -> Nothing
        _ -> wrongError,
    NegTest
      "No Juvix code blocks"
      $(mkRelDir "Markdown")
      $(mkRelFile "NoJuvixCodeBlocks.juvix.md")
      $ \case
        ErrMarkdownBackend (ErrNoJuvixCodeBlocks _) -> Nothing
        _ -> wrongError
  ]
