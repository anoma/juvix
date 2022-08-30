module Scope.Positive where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Pretty qualified as M
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude.Pretty

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
                  (defaultEntryPoint entryFile)
                    { _entryPointRoot = cwd,
                      _entryPointNoStdlib = noStdlib
                    }
                stdlibMap :: HashMap FilePath Text
                stdlibMap = HashMap.mapKeys (cwd </>) (HashMap.fromList stdlibDir)
                unionStdlib :: HashMap FilePath Text -> HashMap FilePath Text
                unionStdlib fs
                  | noStdlib = fs
                  | otherwise = HashMap.union fs stdlibMap

            step "Parsing"
            p :: Parser.ParserResult <- runIO' (upToParsing entryPoint)

            let p2 = head (p ^. Parser.resultModules)

            step "Scoping"
            s :: Scoper.ScoperResult <-
              runIO'
                ( do
                    void (entrySetup entryPoint)
                    Concrete.fromParsed p
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
              (runM . runErrorIO' @JuvixError . runNameIdGen . runFilesPure fs2)
                (upToParsing entryPoint)

            step "Parsing pretty parsed"
            let fs3 = unionStdlib (HashMap.singleton entryFile parsedPretty)
            parsedPretty' :: Parser.ParserResult <-
              (runM . runErrorIO' @JuvixError . runNameIdGen . runFilesPure fs3)
                (upToParsing entryPoint)

            step "Scoping the scoped"
            s' :: Scoper.ScoperResult <-
              (runM . runErrorIO' @JuvixError . runNameIdGen . runFilesPure fs)
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
      "Inductive.juvix",
    PosTest
      "Imports and qualified names"
      "Imports"
      StdlibInclude
      "A.juvix",
    PosTest
      "Data.Bool from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Bool.juvix",
    PosTest
      "Data.Nat from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Nat.juvix",
    PosTest
      "Data.Ord from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Ord.juvix",
    PosTest
      "Data.Product from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/Product.juvix",
    PosTest
      "Data.List and friends from the stdlib"
      "StdlibList"
      StdlibExclude
      "Data/List.juvix",
    PosTest
      "Operators (+)"
      "."
      StdlibExclude
      "Operators.juvix",
    PosTest
      "Literals"
      "."
      StdlibExclude
      "Literals.juvix",
    PosTest
      "Axiom with backends"
      "."
      StdlibExclude
      "Axiom.juvix",
    PosTest
      "Foreign block parsing"
      "."
      StdlibExclude
      "Foreign.juvix",
    PosTest
      "Multiple modules non-ambiguous symbol - same file"
      "QualifiedSymbol"
      StdlibExclude
      "M.juvix",
    PosTest
      "Multiple modules non-ambiguous symbol"
      "QualifiedSymbol2"
      StdlibExclude
      "N.juvix",
    PosTest
      "Multiple modules constructor non-ambiguous symbol"
      "QualifiedConstructor"
      StdlibExclude
      "M.juvix",
    PosTest
      "Parsing"
      "."
      StdlibExclude
      "Parsing.juvix",
    PosTest
      "open overrides open public"
      "."
      StdlibExclude
      "ShadowPublicOpen.juvix",
    PosTest
      "Import embedded standard library"
      "StdlibImport"
      StdlibInclude
      "StdlibImport.juvix",
    PosTest
      "Check Valid Symbols"
      ""
      StdlibInclude
      "Symbols.juvix"
  ]
