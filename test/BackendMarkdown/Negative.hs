module BackendMarkdown.Negative where

import Base
import Juvix.Compiler.Backend.Markdown.Error
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Parser.Error

type FailMsg = String

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _checkErr :: JuvixError -> Maybe FailMsg
  }

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entryPoint <- testDefaultEntryPointIO tRoot file'
            result <- testTaggedLockedToIO (runIOEither entryPoint upToScopingEntry)
            case result of
              Left err -> whenJust (_checkErr err) assertFailure
              Right (_, pipelineRes) -> checkResult pipelineRes
        }
  where
    checkResult :: PipelineResult Scoper.ScoperResult -> IO ()
    checkResult PipelineResult {..} = do
      let m = _pipelineResult ^. Scoper.resultModule
          opts =
            ProcessJuvixBlocksArgs
              { _processJuvixBlocksArgsConcreteOpts = Concrete.defaultOptions,
                _processJuvixBlocksArgsUrlPrefix = "",
                _processJuvixBlocksArgsIdPrefix = "",
                _processJuvixBlocksArgsNoPath = True,
                _processJuvixBlocksArgsExt = ".html",
                _processJuvixBlocksArgsStripPrefix = "",
                _processJuvixBlocksArgsComments = Comments mempty,
                _processJuvixBlocksArgsFolderStructure = False,
                _processJuvixBlocksArgsModule = m,
                _processJuvixBlocksArgsOutputDir =
                  root <//> $(mkRelDir "markdown")
              }
          res = fromJuvixMarkdown' opts
      case res of
        Left err -> whenJust (_checkErr (JuvixError err)) assertFailure
        Right _ -> assertFailure "Unexpected success"

allTests :: TestTree
allTests =
  testGroup
    "Markdown negative tests"
    (map (mkTest . testDescr) tests)

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/negative")

wrongError :: JuvixError -> Maybe FailMsg
wrongError e = Just ("unexpected error: " <> unpack (renderTextDefault e))

tests :: [NegTest]
tests =
  [ NegTest
      "Empty file"
      $(mkRelDir "Markdown")
      $(mkRelFile "Empty.juvix.md")
      $ \e -> case fromJuvixError e of
        Just (ErrMarkdownBackend (ErrNoJuvixCodeBlocks _)) -> Nothing
        _ -> wrongError e,
    NegTest
      "No Juvix code blocks"
      $(mkRelDir "Markdown")
      $(mkRelFile "NoJuvixCodeBlocks.juvix.md")
      $ \e -> case fromJuvixError e of
        Just (ErrMarkdownBackend (ErrNoJuvixCodeBlocks _)) -> Nothing
        _ -> wrongError e,
    NegTest
      "extract-module-statements with no local module"
      $(mkRelDir "Markdown")
      $(mkRelFile "InvalidExtractModuleBlockNoModule.juvix.md")
      $ \e -> case fromJuvixError e of
        Just (ErrInvalidExtractModuleBlock _) -> Nothing
        _ -> wrongError e,
    NegTest
      "extract-module-statements with no local module"
      $(mkRelDir "Markdown")
      $(mkRelFile "InvalidExtractModuleBlockNotJustModule.juvix.md")
      $ \e -> case fromJuvixError e of
        Just (ErrInvalidExtractModuleBlock _) -> Nothing
        _ -> wrongError e,
    NegTest
      "code block with both hide and extract-module-statements"
      $(mkRelDir "Markdown")
      $(mkRelFile "InvalidCodeBlockExtraAttribute.juvix.md")
      $ \e -> case fromJuvixError e of
        Just (ErrInvalidCodeBlockAttribtues _) -> Nothing
        _ -> wrongError e
  ]
