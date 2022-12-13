module Scope.Positive where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Pretty qualified as M
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude.Pretty

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _stdlibMode :: StdlibMode,
    _file :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

renderCode :: M.PrettyCode c => c -> Text
renderCode = prettyText . M.ppOutDefault

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            cwd <- getCurrentDir

            let noStdlib = _stdlibMode == StdlibExclude
                entryPoint =
                  (defaultEntryPoint cwd file')
                    { _entryPointRoot = cwd,
                      _entryPointNoStdlib = noStdlib
                    }

            step "Parsing"
            p :: Parser.ParserResult <- snd <$> runIO' iniState entryPoint upToParsing

            let p2 = head (p ^. Parser.resultModules)

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

            let s2 = head (s ^. Scoper.resultModules)

                fs :: HashMap (Path Abs File) Text
                fs =
                  HashMap.fromList
                    [ (absFile (getModuleFileAbsPath (toFilePath cwd) m), renderCode m)
                      | m <- toList (getAllModules s2)
                    ]

            let scopedPretty = renderCode s2
                parsedPretty = renderCode p2
                runHelper ::
                  HashMap (Path Abs File) Text ->
                  Sem
                    '[ PathResolver,
                       Reader EntryPoint,
                       Files,
                       NameIdGen,
                       Error JuvixError,
                       Reader GenericOptions,
                       Embed IO
                     ]
                    a ->
                  IO a
                runHelper files = runM . runErrorIO' @JuvixError . runNameIdGen . runFilesPure files cwd . runReader entryPoint . runPathResolverPipe

            step "Parsing pretty scoped"
            let fs2 = HashMap.singleton file' scopedPretty
            p' :: Parser.ParserResult <- runHelper fs2 upToParsing

            step "Parsing pretty parsed"
            let fs3 = HashMap.singleton file' parsedPretty
            parsedPretty' :: Parser.ParserResult <- runHelper fs3 upToParsing

            step "Scoping the scoped"
            s' :: Scoper.ScoperResult <- runHelper fs upToScoping

            step "Checks"
            let smodules = s ^. Scoper.resultModules
                smodules' = s' ^. Scoper.resultModules

            let pmodules = p ^. Parser.resultModules
                pmodules' = p' ^. Parser.resultModules
                parsedPrettyModules = parsedPretty' ^. Parser.resultModules

            assertEqDiff "check: scope . parse . pretty . scope . parse = scope . parse" smodules smodules'
            assertEqDiff "check: parse . pretty . scope . parse = parse" pmodules pmodules'
            assertEqDiff "check: parse . pretty . parse = parse" pmodules parsedPrettyModules
        }

allTests :: TestTree
allTests =
  testGroup
    "Scope positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Inductive"
      $(mkRelDir ".")
      StdlibInclude
      $(mkRelFile "Inductive.juvix"),
    PosTest
      "Imports and qualified names"
      $(mkRelDir "Imports")
      StdlibInclude
      $(mkRelFile "A.juvix"),
    PosTest
      "Data.Bool from the stdlib"
      $(mkRelDir "StdlibList")
      StdlibExclude
      $(mkRelFile "Data/Bool.juvix"),
    PosTest
      "Data.Nat from the stdlib"
      $(mkRelDir "StdlibList")
      StdlibExclude
      $(mkRelFile "Data/Nat.juvix"),
    PosTest
      "Data.Ord from the stdlib"
      $(mkRelDir "StdlibList")
      StdlibExclude
      $(mkRelFile "Data/Ord.juvix"),
    PosTest
      "Data.Product from the stdlib"
      $(mkRelDir "StdlibList")
      StdlibExclude
      $(mkRelFile "Data/Product.juvix"),
    PosTest
      "Data.List and friends from the stdlib"
      $(mkRelDir "StdlibList")
      StdlibExclude
      $(mkRelFile "Data/List.juvix"),
    PosTest
      "Operators (+)"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "Operators.juvix"),
    PosTest
      "Literals"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "Literals.juvix"),
    PosTest
      "Axiom with backends"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "Axiom.juvix"),
    PosTest
      "Foreign block parsing"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "Foreign.juvix"),
    PosTest
      "Multiple modules non-ambiguous symbol - same file"
      $(mkRelDir "QualifiedSymbol")
      StdlibExclude
      $(mkRelFile "M.juvix"),
    PosTest
      "Multiple modules non-ambiguous symbol"
      $(mkRelDir "QualifiedSymbol2")
      StdlibExclude
      $(mkRelFile "N.juvix"),
    PosTest
      "Multiple modules constructor non-ambiguous symbol"
      $(mkRelDir "QualifiedConstructor")
      StdlibExclude
      $(mkRelFile "M.juvix"),
    PosTest
      "Parsing"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "Parsing.juvix"),
    PosTest
      "open overrides open public"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "ShadowPublicOpen.juvix"),
    PosTest
      "Infix chains"
      $(mkRelDir ".")
      StdlibInclude
      $(mkRelFile "Ape.juvix"),
    PosTest
      "Import embedded standard library"
      $(mkRelDir "StdlibImport")
      StdlibInclude
      $(mkRelFile "StdlibImport.juvix"),
    PosTest
      "Check Valid Symbols"
      $(mkRelDir ".")
      StdlibInclude
      $(mkRelFile "Symbols.juvix"),
    PosTest
      "Builtin bool"
      $(mkRelDir ".")
      StdlibExclude
      $(mkRelFile "BuiltinsBool.juvix")
  ]
