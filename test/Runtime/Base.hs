module Runtime.Base where

import Base
import Data.FileEmbed
import Data.Text.IO qualified as TIO
import System.IO.Extra (withTempDir)
import System.Process qualified as P

clangCompile ::
  (FilePath -> FilePath -> [String]) ->
  FilePath ->
  FilePath ->
  (FilePath -> IO Text) ->
  (String -> IO ()) ->
  IO Text
clangCompile mkClangArgs inputFile outputFile execute step =
  withTempDir
    ( \dirPath -> do
        let outputFile' = dirPath </> outputFile
        step "C compilation"
        P.callProcess
          "clang"
          (mkClangArgs outputFile' inputFile)
        step "Execution"
        execute outputFile'
    )

clangAssertion :: FilePath -> FilePath -> Text -> ((String -> IO ()) -> Assertion)
clangAssertion inputFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists "clang"
  assertCmdExists "wasmer"

  step "Lookup WASI_SYSROOT_PATH"
  sysrootPath <-
    assertEnvVar
      "Env var WASI_SYSROOT_PATH missing. Set to the location of the wasi-clib sysroot"
      "WASI_SYSROOT_PATH"

  expected <- TIO.readFile expectedFile

  let executeWasm :: FilePath -> IO Text
      executeWasm outputFile = pack <$> P.readProcess "wasmer" [outputFile] (unpack stdinText)

  let executeNative :: FilePath -> IO Text
      executeNative outputFile = pack <$> P.readProcess outputFile [] (unpack stdinText)

  step "Compile C to WASM32-WASI"
  actualWasm <- clangCompile (wasiArgs sysrootPath) inputFile "Program.wasm" executeWasm step
  step "Compare expected and actual program output"
  assertEqDiff ("check: WASM output = " <> expectedFile) actualWasm expected

  step "Compile C to native 64-bit code"
  actualNative <- clangCompile native64Args inputFile "Program" executeNative step
  step "Compare expected and actual program output"
  assertEqDiff ("check: native output = " <> expectedFile) actualNative expected

commonArgs :: FilePath -> [String]
commonArgs outputFile =
  [ "-DDEBUG",
    "-W",
    "-Wall",
    "-Wno-unused-parameter",
    "-Werror",
    "-std=c11",
    "-I",
    runtimeInclude,
    "-o",
    outputFile
  ]
  where
    runtimeInclude :: FilePath
    runtimeInclude = $(makeRelativeToProject "runtime/include" >>= strToExp)

native64Args :: FilePath -> FilePath -> [String]
native64Args outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         "-O3",
         "-L",
         juvixLibraryDir,
         "-ljuvix",
         inputFile
       ]
  where
    juvixLibraryDir :: FilePath
    juvixLibraryDir = $(makeRelativeToProject "runtime/_build.native64-debug" >>= strToExp)

wasiArgs :: FilePath -> FilePath -> FilePath -> [String]
wasiArgs sysrootPath outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         "-Os",
         "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         sysrootPath,
         "-L",
         juvixLibraryDir,
         "-ljuvix",
         inputFile
       ]
  where
    juvixLibraryDir :: FilePath
    juvixLibraryDir = $(makeRelativeToProject "runtime/_build.wasm32-wasi-debug" >>= strToExp)
