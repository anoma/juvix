module Format where

import Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Print qualified as P
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude.Pretty

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

renderCode :: (HasLoc a, P.PrettyPrint a) => P.Comments -> a -> Text
renderCode c = prettyText . P.ppOutDefault c

type Pipe =
  '[ PathResolver,
     Reader EntryPoint,
     Files,
     NameIdGen,
     Error JuvixError,
     Reader GenericOptions,
     Embed IO
   ]

posTest :: String -> Path Rel Dir -> Path Rel File -> PosTest
posTest _name rdir rfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
   in PosTest {..}

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  TestDescr
    { _testName = _name,
      _testRoot = _dir,
      _testAssertion = Steps $ \step -> do
        pkg <- readPackageIO _dir (rootBuildDir _dir)
        let entryPoint = entryPointFromPackage _dir _file pkg
        original :: Text <- readFile (toFilePath (entryPoint ^. entryPointModulePaths . _head1))
        step "Parsing"
        p :: Parser.ParserResult <- snd <$> runIO' iniState entryPoint upToParsing

        step "Scoping"
        s :: Scoper.ScoperResult <-
          snd
            <$> runIO'
              iniState
              entryPoint
              ( do
                  void entrySetup
                  Concrete.fromParsed p
              )

        let formatted :: Text
            formatted = renderCode (s ^. Scoper.comments) (s ^. Scoper.mainModule)

        step "Format"
        assertEqDiffText "check: pretty . scope . parse = id" original formatted
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
  ]
