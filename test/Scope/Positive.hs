module Scope.Positive where

import Base
import Juvix.Compiler.Concrete.Extra
import Juvix.Compiler.Concrete.Print qualified as P
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Store.Scoped.Language
import Juvix.Prelude.Pretty

data PathResolverMode
  = FullPathResolver
  | PackagePathResolver
  deriving stock (Eq)

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _pathResolverMode :: PathResolverMode
  }

posTest :: String -> Path Rel Dir -> Path Rel File -> PosTest
posTest _name _relDir _file = PosTest {_pathResolverMode = FullPathResolver, ..}

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive")

renderCodeNew :: (P.PrettyPrint c) => c -> Text
renderCodeNew = toPlainText . P.ppOutNoComments P.defaultOptions

getModuleFilePath' :: Either ScopedModule (Scoper.Module s 'Scoper.ModuleTop) -> Path Abs File
getModuleFilePath' = \case
  Left m -> m ^. scopedModuleFilePath
  Right m -> getModuleFilePath m

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} = helper renderCodeNew
  where
    helper :: (forall c. (HasLoc c, P.PrettyPrint c) => c -> Text) -> TestDescr
    helper renderer =
      let tRoot = root <//> _relDir
          file' = tRoot <//> _file
       in TestDescr
            { _testName = _name,
              _testRoot = tRoot,
              _testAssertion = Steps $ \step -> do
                entryPoint <- testDefaultEntryPointIO tRoot file'

                let evalHelper :: Text -> Sem (PipelineEff PipelineAppEffects) a -> IO (PipelineResult a)
                    evalHelper input_ m = snd <$> testRunIO entryPoint {_entryPointStdin = Just input_} m

                step "Parsing & Scoping"
                PipelineResult s _ _ <- snd <$> testRunIO entryPoint upToScoping

                let p = s ^. Scoper.resultParserResult
                    fScoped :: Text
                    fScoped = renderer $ s ^. Scoper.resultModule
                    fParsed :: Text
                    fParsed = renderer $ p ^. Parser.resultModule

                step "Parsing & scoping pretty scoped"
                PipelineResult s' _ _ <- evalHelper fScoped upToScoping
                let p' = s' ^. Scoper.resultParserResult

                step "Parsing pretty parsed"
                PipelineResult parsedPretty' _ _ <- evalHelper fParsed upToParsedSource

                step "Checks"
                let smodule = s ^. Scoper.resultModule
                    smodule' = s' ^. Scoper.resultModule

                let pmodule = p ^. Parser.resultModule
                    pmodule' = p' ^. Parser.resultModule
                    parsedPrettyModule = parsedPretty' ^. Parser.resultModule

                assertEqDiffShow "check: scope . parse . pretty . scope . parse = scope . parse" smodule smodule'
                assertEqDiffShow "check: parse . pretty . scope . parse = parse" pmodule pmodule'
                assertEqDiffShow "check: parse . pretty . parse = parse" pmodule parsedPrettyModule
            }

allTests :: TestTree
allTests =
  testGroup
    "Scope positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ posTest
      "Inductive"
      $(mkRelDir ".")
      $(mkRelFile "Inductive.juvix"),
    posTest
      "Pipes symbol as possible prefix for each data constructor"
      $(mkRelDir ".")
      $(mkRelFile "InductivePipes.juvix"),
    posTest
      "Imports and qualified names"
      $(mkRelDir "Imports")
      $(mkRelFile "A.juvix"),
    posTest
      "Data.Bool from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Bool.juvix"),
    posTest
      "Data.Nat from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Nat.juvix"),
    posTest
      "Data.Ord from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Ord.juvix"),
    posTest
      "Data.Product from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/Product.juvix"),
    posTest
      "Data.List and friends from the stdlib"
      $(mkRelDir "StdlibList")
      $(mkRelFile "Data/List.juvix"),
    posTest
      "Operators (+)"
      $(mkRelDir ".")
      $(mkRelFile "Operators.juvix"),
    posTest
      "Literals"
      $(mkRelDir ".")
      $(mkRelFile "Literals.juvix"),
    posTest
      "Axiom with backends"
      $(mkRelDir ".")
      $(mkRelFile "Axiom.juvix"),
    posTest
      "Multiple modules non-ambiguous symbol - same file"
      $(mkRelDir "QualifiedSymbol")
      $(mkRelFile "M.juvix"),
    posTest
      "Multiple modules non-ambiguous symbol"
      $(mkRelDir "QualifiedSymbol2")
      $(mkRelFile "N.juvix"),
    posTest
      "Multiple modules constructor non-ambiguous symbol"
      $(mkRelDir "QualifiedConstructor")
      $(mkRelFile "M.juvix"),
    posTest
      "Parsing"
      $(mkRelDir ".")
      $(mkRelFile "Parsing.juvix"),
    posTest
      "open overrides open public"
      $(mkRelDir ".")
      $(mkRelFile "ShadowPublicOpen.juvix"),
    posTest
      "Infix chains"
      $(mkRelDir ".")
      $(mkRelFile "Ape.juvix"),
    posTest
      "Import embedded standard library"
      $(mkRelDir "StdlibImport")
      $(mkRelFile "StdlibImport.juvix"),
    posTest
      "Basic dependencies"
      $(mkRelDir "Dependencies")
      $(mkRelFile "Input.juvix"),
    posTest
      "Check Valid Symbols"
      $(mkRelDir ".")
      $(mkRelFile "Symbols.juvix"),
    posTest
      "Builtin bool"
      $(mkRelDir ".")
      $(mkRelFile "BuiltinsBool.juvix"),
    posTest
      "Type signature with body"
      $(mkRelDir ".")
      $(mkRelFile "SignatureWithBody.juvix"),
    posTest
      "Case expressions"
      $(mkRelDir "Internal")
      $(mkRelFile "Case.juvix"),
    posTest
      "Qualified imports"
      $(mkRelDir "QualifiedImports")
      $(mkRelFile "Main.juvix"),
    posTest
      "Short syntax for multiple parameters"
      $(mkRelDir ".")
      $(mkRelFile "MultiParams.juvix"),
    posTest
      "Shadow imported symbol"
      $(mkRelDir "ImportShadow")
      $(mkRelFile "Main.juvix"),
    posTest
      "Judoc"
      $(mkRelDir ".")
      $(mkRelFile "Judoc.juvix"),
    posTest
      "Pragmas"
      $(mkRelDir ".")
      $(mkRelFile "Pragmas.juvix"),
    posTest
      "Import as open"
      $(mkRelDir "ImportAsOpen")
      $(mkRelFile "Main.juvix"),
    posTest
      "Iterators"
      $(mkRelDir ".")
      $(mkRelFile "Iterators.juvix"),
    posTest
      "New function syntax"
      $(mkRelDir ".")
      $(mkRelFile "Syntax.juvix"),
    posTest
      "Format pragma"
      $(mkRelDir ".")
      $(mkRelFile "FormatPragma.juvix"),
    posTest
      "Namespaces"
      $(mkRelDir ".")
      $(mkRelFile "Namespaces.juvix"),
    posTest
      "Adt"
      $(mkRelDir ".")
      $(mkRelFile "Adt.juvix"),
    posTest
      "Let open"
      $(mkRelDir ".")
      $(mkRelFile "LetOpen.juvix"),
    PosTest
      "Package file"
      $(mkRelDir "package")
      $(mkRelFile "Package.juvix")
      PackagePathResolver,
    posTest
      "Import nested local module"
      $(mkRelDir "ImportNestedLocalModule")
      $(mkRelFile "ImportNestedLocalModule.juvix")
  ]
