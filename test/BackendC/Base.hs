module BackendC.Base where

import Base
import Data.FileEmbed
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Backend.C.Translation.FromInternal as MiniC
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Pipeline
import System.Process qualified as P

clangCompile ::
  (Path Abs File -> Path Abs File -> [String]) ->
  MiniC.MiniCResult ->
  (Path Abs File -> IO Text) ->
  (String -> IO ()) ->
  IO Text
clangCompile mkClangArgs cResult execute step =
  withTempDir'
    ( \dirPath -> do
        let cOutputFile = dirPath <//> $(mkRelFile "out.c")
            wasmOutputFile = dirPath <//> $(mkRelFile "Input.wasm")
        TIO.writeFile (toFilePath cOutputFile) (cResult ^. MiniC.resultCCode)
        step "WASM generation"
        P.callProcess
          "clang"
          (mkClangArgs wasmOutputFile cOutputFile)
        step "WASM execution"
        execute wasmOutputFile
    )

wasmClangAssertionCGenOnly :: Path Abs File -> ((String -> IO ()) -> Assertion)
wasmClangAssertionCGenOnly mainFile step = do
  step "C Generation"
  root <- getCurrentDir
  let entryPoint = defaultEntryPoint root mainFile
  (void . runIO' iniState entryPoint) upToMiniC

wasmClangAssertion :: WASMInfo -> Path Abs File -> Path Abs File -> ((String -> IO ()) -> Assertion)
wasmClangAssertion WASMInfo {..} mainFile expectedFile step = do
  step "Check clang and wasmer are on path"
  assertCmdExists $(mkRelFile "clang")
  assertCmdExists $(mkRelFile "wasmer")
  root <- getCurrentDir
  step "C Generation"
  let entryPoint = defaultEntryPoint root mainFile
  p :: MiniC.MiniCResult <- snd <$> runIO' iniState entryPoint upToMiniC

  expected <- TIO.readFile (toFilePath expectedFile)

  step "Compile C with wasm standalone runtime"
  actualStandalone <- clangCompile standaloneArgs p _wasmInfoActual step
  step "Compare expected and actual program output"
  assertEqDiffText ("Check: WASM output = " <> toFilePath expectedFile) actualStandalone expected

wasiClangAssertion :: StdlibMode -> Path Abs File -> Path Abs File -> Text -> ((String -> IO ()) -> Assertion)
wasiClangAssertion stdlibMode mainFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists $(mkRelFile "clang")
  assertCmdExists $(mkRelFile "wasmer")

  step "Lookup WASI_SYSROOT_PATH"
  sysrootPath <- getWasiSysrootPath

  root <- getCurrentDir
  step "C Generation"
  let entryPoint = (defaultEntryPoint root mainFile) {_entryPointNoStdlib = stdlibMode == StdlibExclude}
  p :: MiniC.MiniCResult <- snd <$> runIO' iniState entryPoint upToMiniC

  expected <- TIO.readFile (toFilePath expectedFile)

  let execute :: Path Abs File -> IO Text
      execute outputFile = pack <$> P.readProcess "wasmer" [toFilePath outputFile] (unpack stdinText)

  step "Compile C with standalone runtime"
  actualStandalone <- clangCompile (wasiStandaloneArgs sysrootPath) p execute step
  step "Compare expected and actual program output"
  assertEqDiffText ("check: WASM output = " <> toFilePath expectedFile) actualStandalone expected

  step "Compile C with libc runtime"
  actualLibc <- clangCompile (libcArgs sysrootPath) p execute step
  step "Compare expected and actual program output"
  assertEqDiffText ("check: WASM output = " <> toFilePath expectedFile) actualLibc expected

builtinRuntime :: Path Abs Dir
builtinRuntime = absDir $(makeRelativeToProject "c-runtime/builtins" >>= strToExp)

standaloneArgs :: Path Abs File -> Path Abs File -> [String]
standaloneArgs wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    toFilePath (parent minicRuntime),
    "-I",
    toFilePath builtinRuntime,
    "-Werror",
    "--target=wasm32",
    "-nostartfiles",
    "-Wl,--no-entry",
    "-o",
    toFilePath wasmOutputFile,
    toFilePath wallocPath,
    toFilePath cOutputFile
  ]
  where
    minicRuntime :: Path Abs File
    minicRuntime = absFile $(makeRelativeToProject "c-runtime/standalone/c-runtime.h" >>= strToExp)
    wallocPath :: Path Abs File
    wallocPath = absFile $(makeRelativeToProject "c-runtime/walloc/walloc.c" >>= strToExp)

wasiStandaloneArgs :: Path Abs Dir -> Path Abs File -> Path Abs File -> [String]
wasiStandaloneArgs sysrootPath wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    toFilePath (parent minicRuntime),
    "-I",
    toFilePath builtinRuntime,
    "-Werror",
    "--target=wasm32-wasi",
    "--sysroot",
    toFilePath sysrootPath,
    "-o",
    toFilePath wasmOutputFile,
    toFilePath wallocPath,
    toFilePath cOutputFile
  ]
  where
    minicRuntime :: Path Abs File
    minicRuntime = absFile $(makeRelativeToProject "c-runtime/wasi-standalone/c-runtime.h" >>= strToExp)
    wallocPath :: Path Abs File
    wallocPath = absFile $(makeRelativeToProject "c-runtime/walloc/walloc.c" >>= strToExp)

libcArgs :: Path Abs Dir -> Path Abs File -> Path Abs File -> [String]
libcArgs sysrootPath wasmOutputFile cOutputFile =
  [ "-nodefaultlibs",
    "-std=c99",
    "-Oz",
    "-I",
    toFilePath (parent minicRuntime),
    "-I",
    toFilePath builtinRuntime,
    "-Werror",
    "-lc",
    "--target=wasm32-wasi",
    "--sysroot",
    toFilePath sysrootPath,
    "-o",
    toFilePath wasmOutputFile,
    toFilePath cOutputFile
  ]
  where
    minicRuntime :: Path Abs File
    minicRuntime = absFile $(makeRelativeToProject "c-runtime/wasi-libc/c-runtime.h" >>= strToExp)
