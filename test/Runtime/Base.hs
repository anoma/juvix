module Runtime.Base where

import Base
import Data.FileEmbed
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

clangAssertion :: Int -> Path Abs File -> Path Abs File -> Text -> ((String -> IO ()) -> Assertion)
clangAssertion optLevel inputFile expectedFile stdinText step = do
  step "Check clang and wasmer are on path"
  assertCmdExists $(mkRelFile "clang")
  assertCmdExists $(mkRelFile "wasmer")

  step "Lookup WASI_SYSROOT_PATH"
  sysrootPath :: Path Abs Dir <- getWasiSysrootPath

  expected <- readFile expectedFile

  let executeWasm :: Path Abs File -> IO Text
      executeWasm outputFile = readProcess "wasmer" [toFilePath outputFile] stdinText

  let executeNative :: Path Abs File -> IO Text
      executeNative outputFile = readProcess (toFilePath outputFile) [] stdinText

  step "Compile C to WASM32-WASI"
  actualWasm <- clangCompile (wasiArgs optLevel sysrootPath) inputFile $(mkRelFile "Program.wasm") executeWasm step
  step "Compare expected and actual program output"
  assertEqDiffText ("check: WASM output = " <> toFilePath expectedFile) actualWasm expected

  step "Compile C to native 64-bit code"
  actualNative <- clangCompile (native64Args optLevel) inputFile $(mkRelFile "Program") executeNative step
  step "Compare expected and actual program output"
  assertEqDiffText ("check: native output = " <> toFilePath expectedFile) actualNative expected

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
    runtimeInclude = $(makeRelativeToProject "runtime/c/include" >>= strToExp)

native64Args :: Int -> Path Abs File -> Path Abs File -> [String]
native64Args optLevel outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         "-O" <> show optLevel,
         "-L",
         juvixLibraryDir,
         toFilePath inputFile,
         "-ljuvix"
       ]
  where
    juvixLibraryDir :: FilePath
    juvixLibraryDir = $(makeRelativeToProject "runtime/c/_build.native64-debug" >>= strToExp)

wasiArgs :: Int -> Path Abs Dir -> Path Abs File -> Path Abs File -> [String]
wasiArgs optLevel sysrootPath outputFile inputFile =
  commonArgs outputFile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         "-O" <> show optLevel,
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
    juvixLibraryDir = absDir $(makeRelativeToProject "runtime/c/_build.wasm32-wasi-debug" >>= strToExp)
