module Format where

import Base
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline.Setup
import Juvix.Formatter

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Maybe (Path Abs File)
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

posTest :: String -> Path Rel Dir -> Path Rel File -> Maybe (Path Rel File) -> PosTest
posTest _name rdir rfile efile =
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
        entryPoint <- defaultEntryPointCwdIO _file
        let maybeFile = entryPoint ^? entryPointModulePaths . _head
        f <- fromMaybeM (assertFailure "Not a module") (return maybeFile)

        original :: Text <- readFile (toFilePath f)

        step "Parsing"
        p :: Parser.ParserResult <- snd <$> runIO' entryPoint upToParsing

        step "Scoping"
        s :: Scoper.ScoperResult <-
          snd
            <$> runIO'
              entryPoint
              ( do
                  void (entrySetup defaultDependenciesConfig)
                  Concrete.fromParsed p
              )

        case _expectedFile of
          Nothing -> do
            let formatted = formatScoperResult' False original s
            step "Format"
            assertEqDiffText "check: pretty . scope . parse = id" original formatted
          Just eFile -> do
            step "Checking against expected output file"
            let formatted = formatScoperResult' True original s
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
      Nothing,
    posTest
      "TrailingWhitespace"
      $(mkRelDir ".")
      $(mkRelFile "LocalModWithAxiom.juvix")
      (Just $(mkRelFile "LocalModWithAxiom.o"))
  ]
