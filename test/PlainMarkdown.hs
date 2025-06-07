module PlainMarkdown (allTests) where

import Base
import Juvix.Prelude.Pretty
import Markdown.FromSource qualified as Markdown
import Markdown.Language
import Markdown.Print

data PosTest = PosTest
  { _name :: String,
    _file :: Path Rel File
  }

allTests :: TestTree
allTests =
  testGroup
    "PlainMarkdown"
    (map (mkTest . testDescr) tests)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/PlainMarkdown")

renderMd :: (PrettyPrint c) => c -> Text
renderMd = toPlainText . ppOut

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> runM . runSimpleErrorIO . runFilesIO $ do
            liftIO (step "Parsing")
            s :: Blocks <- Markdown.parseFile file'
            let rendered :: Text = renderMd s

            liftIO (step "Parsing pretty parsed")
            parsedPretty <- Markdown.parseText file' rendered

            liftIO (step "Checks")
            liftIO (assertEqDiffShow "parsed . pretty . parsed = parsed" s parsedPretty)
        }

tests :: [PosTest]
tests =
  [ PosTest "benchmark" $(mkRelFile "benchmark.md")
  ]
