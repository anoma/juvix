module Format where

import Base
import Juvix.Formatter

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Maybe (Path Abs File),
    _force :: Bool
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> Maybe (Path Rel File) -> Bool -> PosTest
posTest _name rdir rfile efile _force =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = (_dir <//>) <$> efile
   in PosTest {..}

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Steps $ \step -> do
        entryPoint <- testDefaultEntryPointIO _dir _file
        let maybeFile = entryPoint ^. entryPointModulePath
        f <- fromMaybeM (assertFailure "Not a module") (return maybeFile)

        original :: Text <- readFile (toFilePath f)

        step "Parsing & scoping"
        PipelineResult {..} <- snd <$> testRunIO entryPoint upToScoping

        let formatted = formatScoperResult' _force original _pipelineResult
        case _expectedFile of
          Nothing -> do
            step "Format"
            assertEqDiffText "check: pretty . scope . parse = id" original formatted
          Just eFile -> do
            step "Checking against expected output file"
            expFile :: Text <- readFile (toFilePath eFile)
            assertEqDiffText "Compare to expected output" formatted expFile
    }

allTests :: TestTree
allTests =
  testGroup
    "Format positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ posTest
      "Format"
      $(mkRelDir ".")
      $(mkRelFile "Format.juvix")
      Nothing
      False,
    posTest
      "TrailingWhitespace"
      $(mkRelDir ".")
      $(mkRelFile "LocalModWithAxiom.juvix")
      (Just $(mkRelFile "LocalModWithAxiom.juvix.formatted"))
      True
  ]
