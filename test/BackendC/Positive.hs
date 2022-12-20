module BackendC.Positive where

import BackendC.Base
import Base
import System.Process qualified as P

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _compileMode :: CompileMode
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive/MiniC")

mainFile :: Path Rel File
mainFile = $(mkRelFile "Input.juvix")

expectedFile :: Path Rel File
expectedFile = $(mkRelFile "expected.golden")

actualCallExport :: Text -> [Text] -> Path Abs File -> IO Text
actualCallExport funName funArgs outputFile =
  pack
    <$> P.readProcess
      "wasmer"
      (["run", toFilePath outputFile, "--invoke", unpack funName] <> (unpack <$> funArgs))
      ""

actualCallNode :: Path Rel File -> Path Abs File -> IO Text
actualCallNode jsFile outputFile = do
  assertCmdExists $(mkRelFile "node")
  let outputDir = parent outputFile
      outputJsFile = outputDir <//> jsFile
  copyFile jsFile outputJsFile
  withCurrentDir
    outputDir
    ( pack
        <$> P.readProcess
          "node"
          [toFilePath outputJsFile]
          ""
    )

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      mainFile' = tRoot <//> mainFile
      expectedFile' = tRoot <//> expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ case _compileMode of
            WASI stdlibMode -> wasiClangAssertion stdlibMode mainFile' expectedFile' ""
            WASM i -> wasmClangAssertion i mainFile' expectedFile'
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "HelloWorld" $(mkRelDir "HelloWorld") (WASI StdlibExclude),
    PosTest "Inductive types and pattern matching" $(mkRelDir "Nat") (WASI StdlibExclude),
    PosTest "Polymorphic types" $(mkRelDir "Polymorphism") (WASI StdlibExclude),
    PosTest "Polymorphic axioms" $(mkRelDir "PolymorphicAxioms") (WASI StdlibExclude),
    PosTest "Polymorphic target" $(mkRelDir "PolymorphicTarget") (WASI StdlibExclude),
    PosTest "Multiple modules" $(mkRelDir "MultiModules") (WASI StdlibExclude),
    PosTest "Higher Order Functions" $(mkRelDir "HigherOrder") (WASI StdlibExclude),
    PosTest "Higher Order Functions and explicit holes" $(mkRelDir "PolymorphismHoles") (WASI StdlibExclude),
    PosTest "Closures with no environment" $(mkRelDir "ClosureNoEnv") (WASI StdlibExclude),
    PosTest "Closures with environment" $(mkRelDir "ClosureEnv") (WASI StdlibExclude),
    PosTest "SimpleFungibleTokenImplicit" $(mkRelDir "SimpleFungibleTokenImplicit") (WASI StdlibExclude),
    PosTest "Mutually recursive function" $(mkRelDir "MutuallyRecursive") (WASI StdlibExclude),
    PosTest "Nested List type" $(mkRelDir "NestedList") (WASI StdlibExclude),
    PosTest "Builtin types and functions" $(mkRelDir "Builtins") (WASI StdlibExclude),
    PosTest "Import from embedded standard library" $(mkRelDir "StdlibImport") (WASI StdlibInclude),
    PosTest "Axiom without a compile block" $(mkRelDir "AxiomNoCompile") (WASI StdlibInclude),
    PosTest "Invoke a function using exported name" $(mkRelDir "ExportName") (WASM (WASMInfo (actualCallExport "fun" []))),
    PosTest "Invoke a function using exported name with args" $(mkRelDir "ExportNameArgs") (WASM (WASMInfo (actualCallExport "fun" ["0"]))),
    PosTest "Invoke an imported function in Juvix and exported function in JS" $(mkRelDir "ImportExportName") (WASM (WASMInfo (actualCallNode $(mkRelFile "input.js")))),
    PosTest "Invoke an exported function using Anoma _validate_tx signature" $(mkRelDir "AlwaysValidVP") (WASM (WASMInfo (actualCallNode $(mkRelFile "input.js"))))
  ]
