module BackendC.Base where

import Base
import Data.FileEmbed
import Data.Text.IO qualified as TIO
import Juvix.Pipeline
import Juvix.Translation.MonoJuvixToMiniC as MiniC
import System.IO.Extra (withTempDir)
import System.Process qualified as P

clangCompile ::
  (FilePath -> FilePath -> [String]) ->
  MiniC.MiniCResult ->
  Text ->
  (String -> IO ()) ->
  IO Text
clangCompile mkClangArgs cResult stdinText step =
  withTempDir
    ( \dirPath -> do
        let cOutputFile = dirPath </> "out.c"
            wasmOutputFile = dirPath </> "out.wasm"
        TIO.writeFile cOutputFile (cResult ^. MiniC.resultCCode)
        step "WASM generation"
        P.callProcess
          "clang"
          (mkClangArgs wasmOutputFile cOutputFile)
        step "WASM execution"
        pack <$> P.readProcess "wasmer" [wasmOutputFile] (unpack stdinText)
    )

clangAssertion :: StdlibMode -> FilePath -> FilePath -> Text -> ((String -> IO ()) -> Assertion)
clangAssertion stdlibMode mainFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists "clang"
  assertCmdExists "wasmer"

  step "Lookup WASI_SYSROOT_PATH"
  sysrootPath <-
    assertEnvVar
      "Env var WASI_SYSROOT_PATH missing. Set to the location of the wasi-clib sysroot"
      "WASI_SYSROOT_PATH"

  step "C Generation"
  let entryPoint = (defaultEntryPoint mainFile) {_entryPointNoStdlib = stdlibMode == StdlibExclude}
  p :: MiniC.MiniCResult <- runIO (upToMiniC entryPoint)

  expected <- TIO.readFile expectedFile

  step "Compile C with standalone runtime"
  actualStandalone <- clangCompile (standaloneArgs sysrootPath) p stdinText step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> expectedFile) actualStandalone expected

  step "Compile C with libc runtime"
  actualLibc <- clangCompile (libcArgs sysrootPath) p stdinText step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> expectedFile) actualLibc expected

builtinRuntime :: FilePath
builtinRuntime = $(makeRelativeToProject "minic-runtime/builtins" >>= strToExp)

standaloneArgs :: FilePath -> FilePath -> FilePath -> [String]
standaloneArgs sysrootPath wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    takeDirectory minicRuntime,
    "-I",
    builtinRuntime,
    "-Werror",
    "--target=wasm32-wasi",
    "--sysroot",
    sysrootPath,
    "-o",
    wasmOutputFile,
    wallocPath,
    cOutputFile
  ]
  where
    minicRuntime :: FilePath
    minicRuntime = $(makeRelativeToProject "minic-runtime/standalone/minic-runtime.h" >>= strToExp)
    wallocPath :: FilePath
    wallocPath = $(makeRelativeToProject "minic-runtime/standalone/walloc.c" >>= strToExp)

libcArgs :: FilePath -> FilePath -> FilePath -> [String]
libcArgs sysrootPath wasmOutputFile cOutputFile =
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
    "--sysroot",
    sysrootPath,
    "-o",
    wasmOutputFile,
    cOutputFile
  ]
  where
    minicRuntime :: FilePath
    minicRuntime = $(makeRelativeToProject "minic-runtime/libc/minic-runtime.h" >>= strToExp)
