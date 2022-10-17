module Commands.Dev.Runtime.Compile where

import Commands.Base
import Commands.Dev.Runtime.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import System.Environment
import System.Process qualified as P

runCommand :: forall r. Members '[Embed IO, App] r => RuntimeCompileOptions -> Sem r ()
runCommand opts = do
  root <- askRoot
  let inputFile = opts ^. runtimeCompileInputFile . pathPath
  result <- embed (runCompile root inputFile opts)
  case result of
    Left err -> printFailureExit err
    _ -> return ()

juvixBuildDir :: FilePath
juvixBuildDir = ".juvix-build"

juvixIncludeDir :: FilePath
juvixIncludeDir = juvixBuildDir </> "include"

runCompile :: FilePath -> FilePath -> RuntimeCompileOptions -> IO (Either Text ())
runCompile projRoot inputFile o = do
  createDirectoryIfMissing True (projRoot </> juvixBuildDir)
  createDirectoryIfMissing True (projRoot </> juvixIncludeDir)
  prepareRuntime projRoot o
  case o ^. runtimeCompileTarget of
    TargetWasmWasi -> runM (runError (clangWasmWasiCompile inputFile o))
    TargetNative64 -> runM (runError (clangNativeCompile inputFile o))

prepareRuntime :: FilePath -> RuntimeCompileOptions -> IO ()
prepareRuntime projRoot o = do
  mapM_ writeHeader headersDir
  case o ^. runtimeCompileTarget of
    TargetWasmWasi | o ^. runtimeCompileDebug -> writeRuntime wasiDebugRuntime
    TargetWasmWasi -> writeRuntime wasiReleaseRuntime
    TargetNative64 | o ^. runtimeCompileDebug -> writeRuntime nativeDebugRuntime
    TargetNative64 -> writeRuntime nativeReleaseRuntime
  where
    wasiReleaseRuntime :: BS.ByteString
    wasiReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi/libjuvix.a" >>= FE.embedFile)

    nativeReleaseRuntime :: BS.ByteString
    nativeReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.native64/libjuvix.a" >>= FE.embedFile)

    wasiDebugRuntime :: BS.ByteString
    wasiDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi-debug/libjuvix.a" >>= FE.embedFile)

    nativeDebugRuntime :: BS.ByteString
    nativeDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.native64-debug/libjuvix.a" >>= FE.embedFile)

    headersDir :: [(FilePath, BS.ByteString)]
    headersDir = $(FE.makeRelativeToProject "runtime/include" >>= FE.embedDir)

    writeRuntime :: BS.ByteString -> IO ()
    writeRuntime =
      BS.writeFile (projRoot </> juvixBuildDir </> "libjuvix.a")

    writeHeader :: (FilePath, BS.ByteString) -> IO ()
    writeHeader (filePath, contents) = do
      createDirectoryIfMissing True (projRoot </> juvixIncludeDir </> takeDirectory filePath)
      BS.writeFile (projRoot </> juvixIncludeDir </> filePath) contents

clangNativeCompile ::
  forall r.
  Members '[Embed IO, Error Text] r =>
  FilePath ->
  RuntimeCompileOptions ->
  Sem r ()
clangNativeCompile inputFile o =
  runClang (native64Args o outputFile inputFile)
  where
    outputFile :: FilePath
    outputFile = maybe defaultOutputFile (^. pathPath) (o ^. runtimeCompileOutputFile)

    defaultOutputFile :: FilePath
    defaultOutputFile =
      if
          | o ^. runtimeCompilePreprocess -> takeBaseName inputFile <> ".out.c"
          | o ^. runtimeCompileAssembly -> takeBaseName inputFile <> ".s"
          | otherwise -> takeBaseName inputFile

clangWasmWasiCompile ::
  forall r.
  Members '[Embed IO, Error Text] r =>
  FilePath ->
  RuntimeCompileOptions ->
  Sem r ()
clangWasmWasiCompile inputFile o = clangArgs >>= runClang
  where
    clangArgs :: Sem r [String]
    clangArgs = wasiArgs o outputFile inputFile <$> sysrootEnvVar

    outputFile :: FilePath
    outputFile = maybe defaultOutputFile (^. pathPath) (o ^. runtimeCompileOutputFile)

    defaultOutputFile :: FilePath
    defaultOutputFile =
      if
          | o ^. runtimeCompilePreprocess -> takeBaseName inputFile <> ".out.c"
          | o ^. runtimeCompileAssembly -> takeBaseName inputFile <> ".s"
          | otherwise -> takeBaseName inputFile <> ".wasm"

    sysrootEnvVar :: Sem r String
    sysrootEnvVar =
      fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
      where
        msg :: Text
        msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: RuntimeCompileOptions -> FilePath -> [String]
commonArgs o outputFile =
  ["-E" | o ^. runtimeCompilePreprocess]
    <> ["-S" | o ^. runtimeCompileAssembly]
    <> (if o ^. runtimeCompileDebug then ["-DDEBUG"] else ["-DNDEBUG"])
    <> [ "-W",
         "-Wall",
         "-Wno-unused-parameter",
         "-Werror",
         "-std=c11",
         "-I",
         juvixIncludeDir,
         "-o",
         outputFile
       ]
    <> ( if
             | not (o ^. runtimeCompilePreprocess || o ^. runtimeCompileAssembly) ->
                 [ "-L",
                   juvixBuildDir
                 ]
             | otherwise -> []
       )

native64Args :: RuntimeCompileOptions -> FilePath -> FilePath -> [String]
native64Args o outputFile inputFile =
  commonArgs o outputFile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         "-O3",
         inputFile
       ]
    <> ( if
             | not (o ^. runtimeCompilePreprocess || o ^. runtimeCompileAssembly) ->
                 ["-ljuvix"]
             | otherwise -> []
       )

wasiArgs :: RuntimeCompileOptions -> FilePath -> FilePath -> FilePath -> [String]
wasiArgs o outputFile inputFile sysrootPath =
  commonArgs o outputFile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         "-Os",
         "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         sysrootPath,
         inputFile
       ]
    <> ( if
             | not (o ^. runtimeCompilePreprocess || o ^. runtimeCompileAssembly) ->
                 ["-ljuvix"]
             | otherwise -> []
       )

runClang ::
  Members '[Embed IO, Error Text] r =>
  [String] ->
  Sem r ()
runClang args = do
  (exitCode, _, err) <- embed (P.readProcessWithExitCode "clang" args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> throw (pack err)
