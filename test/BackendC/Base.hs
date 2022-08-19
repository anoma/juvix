module BackendC.Base where

import Base
import Data.FileEmbed
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.C.Translation.FromInternal as MiniC
import Juvix.Compiler.Pipeline
import System.IO.Extra (withTempDir)
import System.Process qualified as P

clangCompile ::
  (FilePath -> FilePath -> [String]) ->
  MiniC.MiniCResult ->
  (FilePath -> IO Text) ->
  (String -> IO ()) ->
  IO Text
clangCompile mkClangArgs cResult execute step =
  withTempDir
    ( \dirPath -> do
        let cOutputFile = dirPath </> "out.c"
            wasmOutputFile = dirPath </> "Input.wasm"
        TIO.writeFile cOutputFile (cResult ^. MiniC.resultCCode)
        step "WASM generation"
        P.callProcess
          "clang"
          (mkClangArgs wasmOutputFile cOutputFile)
        step "WASM execution"
        execute wasmOutputFile
    )

wasmClangAssertionCGenOnly :: FilePath -> ((String -> IO ()) -> Assertion)
wasmClangAssertionCGenOnly mainFile step = do
  step "C Generation"
  let entryPoint = defaultEntryPoint mainFile
  (void . runIO) (upToMiniC entryPoint)

wasmClangAssertion :: WASMInfo -> FilePath -> FilePath -> ((String -> IO ()) -> Assertion)
wasmClangAssertion WASMInfo {..} mainFile expectedFile step = do
  step "Check clang and wasmer are on path"
  assertCmdExists "clang"
  assertCmdExists "wasmer"

  step "C Generation"
  let entryPoint = defaultEntryPoint mainFile
  p :: MiniC.MiniCResult <- runIO (upToMiniC entryPoint)

  expected <- TIO.readFile expectedFile

  step "Compile C with wasm standalone runtime"
  actualStandalone <- clangCompile standaloneArgs p _wasmInfoActual step
  step "Compare expected and actual program output"
  assertEqDiff ("Check: WASM output = " <> expectedFile) actualStandalone expected

wasiClangAssertion :: StdlibMode -> FilePath -> FilePath -> Text -> ((String -> IO ()) -> Assertion)
wasiClangAssertion stdlibMode mainFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists "clang"
  assertCmdExists "wasmer"

  step "C Generation"
  let entryPoint = (defaultEntryPoint mainFile) {_entryPointNoStdlib = stdlibMode == StdlibExclude}
  p :: MiniC.MiniCResult <- runIO (upToMiniC entryPoint)

  expected <- TIO.readFile expectedFile

  let execute :: FilePath -> IO Text
      execute outputFile = pack <$> P.readProcess "wasmer" [outputFile] (unpack stdinText)

  step "Compile C with standalone runtime"
  actualStandalone <- clangCompile wasiStandaloneArgs p execute step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> expectedFile) actualStandalone expected

  step "Compile C with libc runtime"
  actualLibc <- clangCompile libcArgs p execute step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> expectedFile) actualLibc expected

builtinRuntime :: FilePath
builtinRuntime = $(makeRelativeToProject "c-runtime/builtins" >>= strToExp)

standaloneArgs :: FilePath -> FilePath -> [String]
standaloneArgs wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    takeDirectory minicRuntime,
    "-I",
    builtinRuntime,
    "-Werror",
    "--target=wasm32",
    "-nostartfiles",
    "-Wl,--no-entry",
    "-o",
    wasmOutputFile,
    wallocPath,
    cOutputFile
  ]
  where
    minicRuntime :: FilePath
    minicRuntime = $(makeRelativeToProject "c-runtime/standalone/c-runtime.h" >>= strToExp)
    wallocPath :: FilePath
    wallocPath = $(makeRelativeToProject "c-runtime/walloc/walloc.c" >>= strToExp)

wasiStandaloneArgs :: FilePath -> FilePath -> [String]
wasiStandaloneArgs wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    takeDirectory minicRuntime,
    "-I",
    builtinRuntime,
    "-Werror",
    "--target=wasm32-wasi",
    "-o",
    wasmOutputFile,
    wallocPath,
    cOutputFile
  ]
  where
    minicRuntime :: FilePath
    minicRuntime = $(makeRelativeToProject "c-runtime/wasi-standalone/c-runtime.h" >>= strToExp)
    wallocPath :: FilePath
    wallocPath = $(makeRelativeToProject "c-runtime/walloc/walloc.c" >>= strToExp)

libcArgs :: FilePath -> FilePath -> [String]
libcArgs wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    takeDirectory minicRuntime,
    "-I",
    builtinRuntime,
    "-Werror",
    "-lc",
    "--target=wasm32-wasi",
    "-o",
    wasmOutputFile,
    cOutputFile
  ]
  where
    minicRuntime :: FilePath
    minicRuntime = $(makeRelativeToProject "c-runtime/wasi-libc/c-runtime.h" >>= strToExp)
