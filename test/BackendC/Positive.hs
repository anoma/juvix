module BackendC.Positive where

import BackendC.Base
import Base
import System.Process qualified as P

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _compileMode :: CompileMode
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive/MiniC"

mainFile :: FilePath
mainFile = "Input.juvix"

expectedFile :: FilePath
expectedFile = "expected.golden"

actualCallExport :: Text -> [Text] -> FilePath -> IO Text
actualCallExport funName funArgs outputFile =
  pack
    <$> P.readProcess
      "wasmer"
      (["run", outputFile, "--invoke", unpack funName] <> (unpack <$> funArgs))
      ""

actualCallNode :: FilePath -> FilePath -> IO Text
actualCallNode jsFile outputFile = do
  assertCmdExists "node"
  let outputJsFile = takeDirectory outputFile </> jsFile
  copyFile jsFile outputJsFile
  pack
    <$> P.readProcess
      "node"
      [outputJsFile]
      ""

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ case _compileMode of
            WASI stdlibMode -> wasiClangAssertion stdlibMode mainFile expectedFile ""
            WASM i -> wasmClangAssertion i mainFile expectedFile
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "HelloWorld" "HelloWorld" (WASI StdlibExclude),
    PosTest "Inductive types and pattern matching" "Nat" (WASI StdlibExclude),
    PosTest "Polymorphic types" "Polymorphism" (WASI StdlibExclude),
    PosTest "Polymorphic axioms" "PolymorphicAxioms" (WASI StdlibExclude),
    PosTest "Polymorphic target" "PolymorphicTarget" (WASI StdlibExclude),
    PosTest "Multiple modules" "MultiModules" (WASI StdlibExclude),
    PosTest "Higher Order Functions" "HigherOrder" (WASI StdlibExclude),
    PosTest "Higher Order Functions and explicit holes" "PolymorphismHoles" (WASI StdlibExclude),
    PosTest "Closures with no environment" "ClosureNoEnv" (WASI StdlibExclude),
    PosTest "Closures with environment" "ClosureEnv" (WASI StdlibExclude),
    PosTest "SimpleFungibleTokenImplicit" "SimpleFungibleTokenImplicit" (WASI StdlibExclude),
    PosTest "Mutually recursive function" "MutuallyRecursive" (WASI StdlibExclude),
    PosTest "Nested List type" "NestedList" (WASI StdlibExclude),
    PosTest "Builtin types and functions" "Builtins" (WASI StdlibExclude),
    PosTest "Import from embedded standard library" "StdlibImport" (WASI StdlibInclude),
    PosTest "Axiom without a compile block" "AxiomNoCompile" (WASI StdlibInclude),
    PosTest "Invoke a function using exported name" "ExportName" (WASM (WASMInfo (actualCallExport "fun" []))),
    PosTest "Invoke a function using exported name with args" "ExportNameArgs" (WASM (WASMInfo (actualCallExport "fun" ["0"]))),
    PosTest "Invoke an imported function in Juvix and exported function in JS" "ImportExportName" (WASM (WASMInfo (actualCallNode "input.js")))
  ]
