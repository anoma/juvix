module BackendMarkdown.Positive where

import Base
import Juvix.Compiler.Backend.Markdown.Translation.FromTyped.Source
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Setup

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File,
    _UrlPrefix :: Text,
    _IdPrefix :: Text,
    _NoPath :: Bool
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive/Markdown")

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> Text -> Text -> Bool -> PosTest
posTest _name rdir rfile efile _UrlPrefix _IdPrefix _NoPath =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = _dir <//> efile
   in PosTest {..}

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Steps $ \step -> do
        entryPoint <- testDefaultEntryPointIO _dir _file
        step "Parsing"
        PipelineResult p _ <- snd <$> testRunIO entryPoint upToParsing
        step "Scoping"
        PipelineResult s _ <-
          snd
            <$> testRunIO
              entryPoint
              ( do
                  void (entrySetup defaultDependenciesConfig)
                  runReader p Concrete.fromParsed
              )
        let m = s ^. Scoper.resultModule
        let opts =
              ProcessJuvixBlocksArgs
                { _processJuvixBlocksArgsConcreteOpts = Concrete.defaultOptions,
                  _processJuvixBlocksArgsUrlPrefix = _UrlPrefix,
                  _processJuvixBlocksArgsIdPrefix = _IdPrefix,
                  _processJuvixBlocksArgsNoPath = _NoPath,
                  _processJuvixBlocksArgsComments =
                    Scoper.getScoperResultComments s,
                  _processJuvixBlocksArgsModule = m,
                  _processJuvixBlocksArgsOutputDir =
                    root <//> $(mkRelDir "markdown")
                }

        let res = fromJuvixMarkdown' opts
        case res of
          Left err -> assertFailure (show err)
          Right md -> do
            step "Checking against expected output file"
            expFile :: Text <- readFile (toFilePath _expectedFile)
            assertEqDiffText "Compare to expected output" md expFile
    }

allTests :: TestTree
allTests =
  testGroup
    "Markdown positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ posTest
      "Test Markdown"
      $(mkRelDir ".")
      $(mkRelFile "Test.juvix.md")
      -- Cmd to regenerate the expected file: juvix markdown --prefix-url X --prefix-id Y --no-path Test.juvix.md
      $(mkRelFile "markdown/Test.md")
      "X"
      "Y"
      True
  ]
