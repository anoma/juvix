module BackendC.Positive where

import Base
import Data.Text.IO qualified as TIO
import MiniJuvix.Pipeline
import MiniJuvix.Translation.MonoJuvixToMiniC as MiniC
import System.IO.Extra (withTempDir)
import System.Process qualified as P

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath
  }

makeLenses ''PosTest

root :: FilePath
root = "tests/positive/MiniC"

mainFile :: FilePath
mainFile = "Input.mjuvix"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            step "Check emscripten and wasmer are on path"
            assertCmdExists "emcc"
            assertCmdExists "wasmer"

            step "C Generation"
            let entryPoint = EntryPoint "." (return "Input.mjuvix")
            p :: MiniC.MiniCResult <- runIO (upToMiniC entryPoint)

            actual <-
              withTempDir
                ( \dirPath -> do
                    let cOutputFile = dirPath </> "out.c"
                        wasmOutputFile = dirPath </> "out.wasm"
                    TIO.writeFile cOutputFile (p ^. MiniC.resultCCode)
                    step "WASM generation"
                    P.callProcess "emcc" ["-o", wasmOutputFile, cOutputFile]
                    step "WASM execution"
                    pack <$> P.readProcess "wasmer" [wasmOutputFile] ""
                )

            expected <- TIO.readFile "expected.golden"
            step "Compare expected and actual program output"
            assertEqDiff "check: WASM output = expected.golden" actual expected
        }

allTests :: TestTree
allTests =
  testGroup
    "Backend C positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "HelloWorld" "HelloWorld",
    PosTest "Inductive types and pattern matching" "Nat",
    PosTest "Polymorphic types" "Polymorphism"
  ]
