module Commands.Extra.Compile where

import Commands.Base
import Commands.Extra.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Extra.Paths
import System.Environment
import System.Process qualified as P

runCommand :: forall r. Members '[Embed IO, App] r => CompileOptions -> Sem r ()
runCommand opts = do
  root <- askPkgDir
  inputFile <- someBaseToAbs' (opts ^. compileInputFile . pathPath)
  result <- runCompile root inputFile opts
  case result of
    Left err -> printFailureExit err
    _ -> return ()

runCompile :: Members '[App, Embed IO] r => Path Abs Dir -> Path Abs File -> CompileOptions -> Sem r (Either Text ())
runCompile projRoot inputFile o = do
  ensureDir (projRoot <//> juvixBuildDir)
  ensureDir (projRoot <//> juvixIncludeDir)
  prepareRuntime projRoot o
  case o ^. compileTarget of
    TargetWasm32Wasi -> runError (clangWasmWasiCompile inputFile o)
    TargetNative64 -> runError (clangNativeCompile inputFile o)
    TargetC -> return $ Right ()

prepareRuntime :: forall r. Members '[App, Embed IO] r => Path Abs Dir -> CompileOptions -> Sem r ()
prepareRuntime projRoot o = do
  mapM_ writeHeader headersDir
  case o ^. compileTarget of
    TargetWasm32Wasi | o ^. compileDebug -> writeRuntime wasiDebugRuntime
    TargetWasm32Wasi -> writeRuntime wasiReleaseRuntime
    TargetNative64 | o ^. compileDebug -> writeRuntime nativeDebugRuntime
    TargetNative64 -> writeRuntime nativeReleaseRuntime
    TargetC -> return ()
  where
    wasiReleaseRuntime :: BS.ByteString
    wasiReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi/libjuvix.a" >>= FE.embedFile)

    nativeReleaseRuntime :: BS.ByteString
    nativeReleaseRuntime = $(FE.makeRelativeToProject "runtime/_build.native64/libjuvix.a" >>= FE.embedFile)

    wasiDebugRuntime :: BS.ByteString
    wasiDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.wasm32-wasi-debug/libjuvix.a" >>= FE.embedFile)

    nativeDebugRuntime :: BS.ByteString
    nativeDebugRuntime = $(FE.makeRelativeToProject "runtime/_build.native64-debug/libjuvix.a" >>= FE.embedFile)

    writeRuntime :: BS.ByteString -> Sem r ()
    writeRuntime =
      embed
        . BS.writeFile (toFilePath (projRoot <//> juvixBuildDir <//> $(mkRelFile "libjuvix.a")))

    headersDir :: [(Path Rel File, BS.ByteString)]
    headersDir = map (first relFile) $(FE.makeRelativeToProject "runtime/include" >>= FE.embedDir)

    writeHeader :: (Path Rel File, BS.ByteString) -> Sem r ()
    writeHeader (filePath, contents) = embed $ do
      ensureDir (projRoot <//> juvixIncludeDir <//> parent filePath)
      BS.writeFile (toFilePath (projRoot <//> juvixIncludeDir <//> filePath)) contents

clangNativeCompile ::
  forall r.
  Members '[App, Embed IO, Error Text] r =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangNativeCompile inputFile o = do
  outputFile' <- outputFile
  runClang (native64Args o outputFile' inputFile)
  where
    outputFile :: Sem r (Path Abs File)
    outputFile = maybe (return defaultOutputFile) someBaseToAbs' (o ^? compileOutputFile . _Just . pathPath)

    defaultOutputFile :: Path Abs File
    defaultOutputFile
      | o ^. compilePreprocess = replaceExtension' ".out.c" inputFile
      | o ^. compileAssembly = replaceExtension' ".s" inputFile
      | otherwise = removeExtension' inputFile

clangWasmWasiCompile ::
  forall r.
  Members '[App, Embed IO, Error Text] r =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangWasmWasiCompile inputFile o = clangArgs >>= runClang
  where
    clangArgs :: Sem r [String]
    clangArgs = do
      outputFile' <- outputFile
      wasiArgs o outputFile' inputFile <$> sysrootEnvVar

    outputFile :: Sem r (Path Abs File)
    outputFile = case o ^? compileOutputFile . _Just . pathPath of
      Just f -> someBaseToAbs' f
      Nothing -> return defaultOutputFile

    defaultOutputFile :: Path Abs File
    defaultOutputFile = replaceExtension' extension inputFile
      where
        extension :: String
        extension
          | o ^. compilePreprocess = ".out.c"
          | o ^. compileAssembly = ".wat"
          | otherwise = ".wasm"

    sysrootEnvVar :: Sem r (Path Abs Dir)
    sysrootEnvVar =
      absDir
        <$> fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
      where
        msg :: Text
        msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: CompileOptions -> Path Abs File -> [String]
commonArgs o outputFile =
  ["-E" | o ^. compilePreprocess]
    <> ["-S" | o ^. compileAssembly]
    <> (if o ^. compileDebug then ["-DDEBUG"] else ["-DNDEBUG"])
    <> [ "-W",
         "-Wall",
         "-Wno-unused-parameter",
         "-Wno-unused-label",
         "-Werror",
         "-std=c11",
         "-I",
         toFilePath juvixIncludeDir,
         "-o",
         toFilePath outputFile
       ]
    <> ( if
             | not (o ^. compilePreprocess || o ^. compileAssembly) ->
                 [ "-L",
                   toFilePath juvixBuildDir
                 ]
             | otherwise -> []
       )

native64Args :: CompileOptions -> Path Abs File -> Path Abs File -> [String]
native64Args o outputFile inputFile =
  commonArgs o outputFile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         "-O3",
         toFilePath inputFile
       ]
    <> ( if
             | not (o ^. compilePreprocess || o ^. compileAssembly) ->
                 ["-ljuvix"]
             | otherwise -> []
       )

wasiArgs :: CompileOptions -> Path Abs File -> Path Abs File -> Path Abs Dir -> [String]
wasiArgs o outputFile inputFile sysrootPath =
  commonArgs o outputFile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         "-Os",
         "-nodefaultlibs",
         "--target=wasm32-wasi",
         "--sysroot",
         toFilePath sysrootPath,
         toFilePath inputFile
       ]
    <> ( if
             | not (o ^. compilePreprocess || o ^. compileAssembly) ->
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
