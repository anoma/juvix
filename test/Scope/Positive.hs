module Scope.Positive where

import Base
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Pipeline
import MiniJuvix.Pipeline.Setup
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Parser qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as M
import MiniJuvix.Syntax.Concrete.Scoped.Scoper qualified as Scoper
import MiniJuvix.Syntax.Concrete.Scoped.Utils

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _stdlibMode :: StdlibMode,
    _file :: FilePath
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive"

renderCode :: M.PrettyCode c => c -> Text
renderCode = prettyText . M.ppOutDefault

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            cwd <- getCurrentDirectory
            entryFile <- canonicalizePath _file
            let noStdlib = _stdlibMode == StdlibExclude
                entryPoint =
                  EntryPoint
                    { _entryPointRoot = cwd,
                      _entryPointNoTermination = False,
                      _entryPointNoStdlib = noStdlib,
                      _entryPointModulePaths = pure entryFile
                    }
                stdlibMap :: HashMap FilePath Text
                stdlibMap = HashMap.mapKeys (cwd </>) (HashMap.fromList stdlibDir)
                unionStdlib :: HashMap FilePath Text -> HashMap FilePath Text
                unionStdlib fs
                  | noStdlib = fs
                  | otherwise = HashMap.union fs stdlibMap

            step "Parsing"
            p :: Parser.ParserResult <- runIO (upToParsing entryPoint)

            let p2 = head (p ^. Parser.resultModules)

            step "Scoping"
            s :: Scoper.ScoperResult <-
              runIO
                ( do
                    void (entrySetup entryPoint)
                    pipelineScoper p
                )

            let s2 = head (s ^. Scoper.resultModules)

                fs :: HashMap FilePath Text
                fs =
                  unionStdlib
                    ( HashMap.fromList
                        [ (getModuleFileAbsPath cwd m, renderCode m)
                          | m <- toList (getAllModules s2)
                        ]
                    )

            let scopedPretty = renderCode s2
                parsedPretty = renderCode p2

            step "Parsing pretty scoped"
            let fs2 = unionStdlib (HashMap.singleton entryFile scopedPretty)
            p' :: Parser.ParserResult <-
              (runM . runErrorIO @MiniJuvixError . runNameIdGen . runFilesPure fs2)
                (upToParsing entryPoint)

            step "Parsing pretty parsed"
            let fs3 = unionStdlib (HashMap.singleton entryFile parsedPretty)
            parsedPretty' :: Parser.ParserResult <-
              (runM . runErrorIO @MiniJuvixError . runNameIdGen . runFilesPure fs3)
                (upToParsing entryPoint)

            step "Scoping the scoped"
            s' :: Scoper.ScoperResult <-
              (runM . runErrorIO @MiniJuvixError . runNameIdGen . runFilesPure fs)
                (upToScoping entryPoint)

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
      "."
      StdlibInclude
      "Inductive.mjuvix",
    PosTest
      "Imports and qualified names"
      "Imports"
      StdlibInclude
      "A.mjuvix",
    PosTest
      "Data.Bool from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Bool.mjuvix",
    PosTest
      "Data.Nat from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Nat.mjuvix",
    PosTest
      "Data.Ord from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Ord.mjuvix",
    PosTest
      "Data.Product from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Product.mjuvix",
    PosTest
      "Data.List and friends from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/List.mjuvix",
    PosTest
      "Operators (+)"
      "."
      StdlibExclude
      "Operators.mjuvix",
    PosTest
      "Literals"
      "."
      StdlibExclude
      "Literals.mjuvix",
    PosTest
      "Hello World backends"
      "."
      StdlibExclude
      "HelloWorld.mjuvix",
    PosTest
      "Axiom with backends"
      "."
      StdlibExclude
      "Axiom.mjuvix",
    PosTest
      "Foreign block parsing"
      "."
      StdlibExclude
      "Foreign.mjuvix",
    PosTest
      "Multiple modules non-ambiguous symbol - same file"
      "QualifiedSymbol"
      StdlibExclude
      "M.mjuvix",
    PosTest
      "Multiple modules non-ambiguous symbol"
      "QualifiedSymbol2"
      StdlibExclude
      "N.mjuvix",
    PosTest
      "Multiple modules constructor non-ambiguous symbol"
      "QualifiedConstructor"
      StdlibExclude
      "M.mjuvix",
    PosTest
      "Parsing"
      "."
      StdlibExclude
      "Parsing.mjuvix",
    PosTest
      "open overrides open public"
      "."
      StdlibExclude
      "ShadowPublicOpen.mjuvix",
    PosTest
      "Import embedded standard library"
      "StdlibImport"
      StdlibInclude
      "StdlibImport.mjuvix"
  ]
