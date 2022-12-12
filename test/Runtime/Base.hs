module Runtime.Base where

import Base
import Data.FileEmbed
import Data.Text.IO qualified as TIO
import System.Process qualified as P

clangCompile ::
  (Path Abs File -> Path Abs File -> [String]) ->
  Path Abs File ->
  Path Rel File ->
  (Path Abs File -> IO Text) ->
  (String -> IO ()) ->
  IO Text
clangCompile mkClangArgs inputFile outputFile execute step =
  withTempDir'
    ( \dirPath -> do
        let outputFile' = dirPath <//> outputFile
        step "C compilation"
        P.callProcess
          "clang"
          (mkClangArgs outputFile' inputFile)
        step "Execution"
        execute outputFile'
    )

clangAssertion :: Path Abs File -> Path Abs File -> Text -> ((String -> IO ()) -> Assertion)
clangAssertion inputFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists $(mkRelFile "clang")
  assertCmdExists $(mkRelFile "wasmer")

  step "Lookup WASI_SYSROOT_PATH"
  sysrootPath :: Path Abs Dir <- absDir <$>
    assertEnvVar
      "Env var WASI_SYSROOT_PATH missing. Set to the location of the wasi-clib sysroot"
      "WASI_SYSROOT_PATH"

  expected <- TIO.readFile (toFilePath expectedFile)

  let executeWasm :: Path Abs File -> IO Text
      executeWasm outputFile = pack <$> P.readProcess "wasmer" [toFilePath outputFile] (unpack stdinText)

  let executeNative :: Path Abs File -> IO Text
      executeNative outputFile = pack <$> P.readProcess (toFilePath outputFile) [] (unpack stdinText)

  step "Compile C to WASM32-WASI"
  actualWasm <- clangCompile (wasiArgs sysrootPath) inputFile $(mkRelFile "Program.wasm") executeWasm step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> toFilePath expectedFile) actualWasm expected

  step "Compile C to native 64-bit code"
  actualNative <- clangCompile native64Args inputFile $(mkRelFile "Program") executeNative step
  step "Compare expected and actual program output"
  assertEqDiff ("check: native output = " <> toFilePath expectedFile) actualNative expected

commonArgs :: Path Abs File -> [String]
commonArgs outputFile =
  [ "-DDEBUG",
    "-W",
    "-Wall",
    "-Wno-unused-parameter",
    "-Wno-unused-label",
    "-Werror",
    "-std=c11",
    "-I",
    runtimeInclude,
    "-o",
    toFilePath outputFile
  ]
  where
    runtimeInclude :: FilePath
    runtimeInclude = $(makeRelativeToProject "runtime/include" >>= strToExp)

native64Args :: Path Abs File -> Path Abs File -> [String]
native64Args outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         "-O3",
         "-L",
         juvixLibraryDir,
         toFilePath inputFile,
         "-ljuvix"
       ]
  where
    juvixLibraryDir :: FilePath
    juvixLibraryDir = $(makeRelativeToProject "runtime/_build.native64-debug" >>= strToExp)

wasiArgs :: Path Abs Dir -> Path Abs File -> Path Abs File -> [String]
wasiArgs sysrootPath outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         "-Os",
         "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         toFilePath sysrootPath,
         "-L",
         toFilePath juvixLibraryDir,
         toFilePath inputFile,
         "-ljuvix"
       ]
  where
    juvixLibraryDir :: Path Abs Dir
    juvixLibraryDir = absDir $(makeRelativeToProject "runtime/_build.wasm32-wasi-debug" >>= strToExp)
