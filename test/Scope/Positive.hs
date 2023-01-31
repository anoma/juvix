module Scope.Positive where

import Base
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Concrete qualified as Concrete
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Pretty qualified as M
import Juvix.Compiler.Concrete.Print qualified as P
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Setup
import Juvix.Prelude.Aeson
import Juvix.Prelude.Pretty

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

renderCodeOld :: M.PrettyCode c => c -> Text
renderCodeOld = prettyText . M.ppOutDefault

renderCodeNew :: (HasLoc c, P.PrettyPrint c) => c -> Text
renderCodeNew = prettyText . P.ppOutDefault emptyComments

type Pipe =
  '[ PathResolver,
     Reader EntryPoint,
     Files,
     NameIdGen,
     Error JuvixError,
     Reader GenericOptions,
     Embed IO
   ]

testDescr :: PosTest -> [TestDescr]
testDescr PosTest {..} = helper renderCodeOld "" : [helper renderCodeNew " (with comments)"]
  where
    helper :: (forall c. (HasLoc c, P.PrettyPrint c, M.PrettyCode c) => c -> Text) -> String -> TestDescr
    helper renderer tag =
      let tRoot = root <//> _relDir
          file' = tRoot <//> _file
       in TestDescr
            { _testName = _name <> tag,
              _testRoot = tRoot,
              _testAssertion = Steps $ \step -> do
                pkg <- readPackageIO tRoot (rootBuildDir tRoot)
                let entryPoint = entryPointFromPackage tRoot file' pkg
                    runHelper :: HashMap (Path Abs File) Text -> Sem Pipe a -> IO (ResolverState, a)
                    runHelper files =
                      runM
                        . runErrorIO' @JuvixError
                        . runNameIdGen
                        . runFilesPure files tRoot
                        . runReader entryPoint
                        . runPathResolverPipe
                    evalHelper :: HashMap (Path Abs File) Text -> Sem Pipe a -> IO a
                    evalHelper files = fmap snd . runHelper files

                step "Parsing"
                p :: Parser.ParserResult <- snd <$> runIO' iniState entryPoint upToParsing

                step "Scoping"
                (artif :: Artifacts, s :: Scoper.ScoperResult) <-
                  runIO'
                    iniState
                    entryPoint
                    ( do
                        void entrySetup
                        Concrete.fromParsed p
                    )

                let yamlFiles :: [(Path Abs File, Text)]
                    yamlFiles =
                      [ (pkgi ^. packageRoot <//> juvixYamlFile, encodeToText (rawPackage (pkgi ^. packagePackage)))
                        | pkgi <- toList (artif ^. artifactResolver . resolverPackages)
                      ]
                    fsScoped :: HashMap (Path Abs File) Text
                    fsScoped =
                      HashMap.fromList $
                        [ (getModuleFilePath m, renderer m)
                          | m <- toList (s ^. Scoper.resultScoperTable . Scoper.infoModules)
                        ]
                          <> yamlFiles
                    fsParsed :: HashMap (Path Abs File) Text
                    fsParsed =
                      HashMap.fromList $
                        [ (getModuleFilePath m, renderCodeOld m)
                          | m <- toList (p ^. Parser.resultTable . Parser.infoParsedModules)
                        ]
                          <> yamlFiles

                step "Parsing pretty scoped"
                p' :: Parser.ParserResult <- evalHelper fsScoped upToParsing

                step "Parsing pretty parsed"
                parsedPretty' :: Parser.ParserResult <- evalHelper fsParsed upToParsing

                step "Scoping the scoped"
                s' :: Scoper.ScoperResult <- evalHelper fsScoped upToScoping

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
    (map mkTest (concatMap testDescr tests))

tests :: [PosTest]
tests =
  [ PosTest
      "Inductive"
      $(mkRelDir ".")
      $(mkRelFile "Inductive.juvix"),
    PosTest
      "Pipes symbol as possible prefix for each data constructor"
      $(mkRelDir ".")
      $(mkRelFile "InductivePipes.juvix"),
    PosTest
      "Imports and qualified names"
      $(mkRelDir "Imports")
      $(mkRelFile "A.juvix"),
    PosTest
      "Data.Bool from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Bool.juvix"),
    PosTest
      "Data.Nat from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Nat.juvix"),
    PosTest
      "Data.Ord from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Ord.juvix"),
    PosTest
      "Data.Product from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Product.juvix"),
    PosTest
      "Data.List and friends from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/List.juvix"),
    PosTest
      "Operators (+)"
      $(mkRelDir ".")
      $(mkRelFile "Operators.juvix"),
    PosTest
      "Literals"
      $(mkRelDir ".")
      $(mkRelFile "Literals.juvix"),
    PosTest
      "Axiom with backends"
      $(mkRelDir ".")
      $(mkRelFile "Axiom.juvix"),
    PosTest
      "Foreign block parsing"
      $(mkRelDir ".")
      $(mkRelFile "Foreign.juvix"),
    PosTest
      "Multiple modules non-ambiguous symbol - same file"
      $(mkRelDir "QualifiedSymbol")
      $(mkRelFile "M.juvix"),
    PosTest
      "Multiple modules non-ambiguous symbol"
      $(mkRelDir "QualifiedSymbol2")
      $(mkRelFile "N.juvix"),
    PosTest
      "Multiple modules constructor non-ambiguous symbol"
      $(mkRelDir "QualifiedConstructor")
      $(mkRelFile "M.juvix"),
    PosTest
      "Parsing"
      $(mkRelDir ".")
      $(mkRelFile "Parsing.juvix"),
    PosTest
      "open overrides open public"
      $(mkRelDir ".")
      $(mkRelFile "ShadowPublicOpen.juvix"),
    PosTest
      "Infix chains"
      $(mkRelDir ".")
      $(mkRelFile "Ape.juvix"),
    PosTest
      "Import embedded standard library"
      $(mkRelDir "StdlibImport")
      $(mkRelFile "StdlibImport.juvix"),
    PosTest
      "Basic dependencies"
      $(mkRelDir "Dependencies")
      $(mkRelFile "Input.juvix"),
    PosTest
      "Check Valid Symbols"
      $(mkRelDir ".")
      $(mkRelFile "Symbols.juvix"),
    PosTest
      "Builtin bool"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinsBool.juvix"),
    PosTest
      "Type signature with body"
      $(mkRelDir ".")
      $(mkRelFile "SignatureWithBody.juvix")
  ]
