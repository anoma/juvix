-- DEPRECATED -- TODO remove file
module Commands.Extra.Compile where

import Commands.Base
import Commands.Extra.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Extra.Paths
import System.Environment
import System.Process qualified as P

runCommandMain :: forall r. (Members '[EmbedIO, App] r) => CompileOptionsMain -> Sem r ()
runCommandMain = fromCompileOptionsMain >=> runCommand

runCommand :: forall r. (Members '[EmbedIO, App] r) => CompileOptions -> Sem r ()
runCommand = runCompile

runCompile ::
  (Members '[App, EmbedIO] r) =>
  CompileOptions ->
  Sem r ()
runCompile opts = do
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  prepareRuntime buildDir opts
  case opts ^. compileTarget of
    AppTargetWasm32Wasi -> clangWasmWasiCompile opts
    AppTargetNative64 -> clangNativeCompile opts
    AppTargetVampIR -> return ()
    AppTargetCore -> return ()
    AppTargetAsm -> return ()
    AppTargetReg -> return ()
    AppTargetTree -> return ()
    AppTargetAnoma -> return ()
    AppTargetCasm -> return ()
    AppTargetCairo -> return ()
    AppTargetRiscZeroRust -> return ()

prepareRuntime :: forall r. (Members '[App, EmbedIO] r) => Path Abs Dir -> CompileOptions -> Sem r ()
prepareRuntime buildDir o = do
  mapM_ writeHeader headersDir
  case o ^. compileTarget of
    AppTargetWasm32Wasi
      | o ^. compileDebug -> writeRuntime wasiDebugRuntime
    AppTargetWasm32Wasi -> writeRuntime wasiReleaseRuntime
    AppTargetNative64
      | o ^. compileDebug -> writeRuntime nativeDebugRuntime
    AppTargetNative64 -> writeRuntime nativeReleaseRuntime
    AppTargetVampIR -> return ()
    AppTargetCore -> return ()
    AppTargetAsm -> return ()
    AppTargetReg -> return ()
    AppTargetTree -> return ()
    AppTargetAnoma -> return ()
    AppTargetCasm -> return ()
    AppTargetCairo -> return ()
    AppTargetRiscZeroRust -> return ()
  where
    wasiReleaseRuntime :: BS.ByteString
    wasiReleaseRuntime = $(FE.makeRelativeToProject "runtime/c/_build.wasm32-wasi/libjuvix.a" >>= FE.embedFile)

    nativeReleaseRuntime :: BS.ByteString
    nativeReleaseRuntime = $(FE.makeRelativeToProject "runtime/c/_build.native64/libjuvix.a" >>= FE.embedFile)

    wasiDebugRuntime :: BS.ByteString
    wasiDebugRuntime = $(FE.makeRelativeToProject "runtime/c/_build.wasm32-wasi-debug/libjuvix.a" >>= FE.embedFile)

    nativeDebugRuntime :: BS.ByteString
    nativeDebugRuntime = $(FE.makeRelativeToProject "runtime/c/_build.native64-debug/libjuvix.a" >>= FE.embedFile)

    writeRuntime :: BS.ByteString -> Sem r ()
    writeRuntime =
      liftIO
        . BS.writeFile (toFilePath (buildDir <//> $(mkRelFile "libjuvix.a")))

    headersDir :: [(Path Rel File, BS.ByteString)]
    headersDir = map (first relFile) $(FE.makeRelativeToProject "runtime/c/include" >>= FE.embedDir)

    includeDir :: Path Abs Dir
    includeDir = juvixIncludeDir buildDir

    writeHeader :: (Path Rel File, BS.ByteString) -> Sem r ()
    writeHeader (filePath, contents) = liftIO $ do
      ensureDir (includeDir <//> parent filePath)
      BS.writeFile (toFilePath (includeDir <//> filePath)) contents

outputFile :: forall r. (Member App r) => CompileOptions -> Sem r (Path Abs File)
outputFile opts = do
  inputfile <- getMainFile (Just (opts ^. compileInputFile))
  maybe (defaultOutputFile inputfile) fromAppPathFile (opts ^? compileOutputFile . _Just)
  where
    defaultOutputFile :: Path Abs File -> Sem r (Path Abs File)
    defaultOutputFile inputFile = do
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ case opts ^. compileTarget of
        AppTargetNative64
          | opts ^. compileCOutput -> replaceExtension' cFileExt inputFile
          | opts ^. compilePreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
          | opts ^. compileAssembly -> replaceExtension' ".s" inputFile
          | otherwise -> removeExtension' baseOutputFile
        AppTargetWasm32Wasi
          | opts ^. compileCOutput -> replaceExtension' cFileExt inputFile
          | opts ^. compilePreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
          | opts ^. compileAssembly -> replaceExtension' ".wat" inputFile
          | otherwise -> replaceExtension' ".wasm" baseOutputFile
        AppTargetVampIR ->
          replaceExtension' vampIRFileExt baseOutputFile
        AppTargetCore ->
          replaceExtension' juvixCoreFileExt baseOutputFile
        AppTargetAsm ->
          replaceExtension' juvixAsmFileExt baseOutputFile
        AppTargetReg ->
          replaceExtension' juvixRegFileExt baseOutputFile
        AppTargetTree ->
          replaceExtension' juvixTreeFileExt baseOutputFile
        AppTargetAnoma ->
          replaceExtension' nockmaFileExt baseOutputFile
        AppTargetCasm ->
          replaceExtension' casmFileExt baseOutputFile
        AppTargetCairo ->
          replaceExtension' jsonFileExt baseOutputFile
        AppTargetRiscZeroRust ->
          replaceExtension' rustFileExt baseOutputFile

clangNativeCompile ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  CompileOptions ->
  Sem r ()
clangNativeCompile o = do
  inputFile <- getMainFile (Just (o ^. compileInputFile))
  outputFile' <- outputFile o
  buildDir <- askBuildDir
  if
    | o ^. compileCOutput ->
        copyFile inputFile outputFile'
    | otherwise ->
        runClang (native64Args buildDir o outputFile' inputFile)

clangWasmWasiCompile ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  CompileOptions ->
  Sem r ()
clangWasmWasiCompile o = do
  inputFile <- getMainFile (Just (o ^. compileInputFile))
  outputFile' <- outputFile o
  buildDir <- askBuildDir
  if
    | o ^. compileCOutput ->
        copyFile inputFile outputFile'
    | otherwise -> do
        clangArgs <- wasiArgs buildDir o outputFile' inputFile <$> sysrootEnvVar
        runClang clangArgs
  where
    sysrootEnvVar :: Sem r (Path Abs Dir)
    sysrootEnvVar =
      absDir
        <$> fromMaybeM (exitFailMsg msg) (liftIO (lookupEnv "WASI_SYSROOT_PATH"))
      where
        msg :: Text
        msg = "Missing environment variable WASI_SYSROOT_PATH"

commonArgs :: Path Abs Dir -> CompileOptions -> Path Abs File -> [String]
commonArgs buildDir o outfile =
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
         toFilePath (juvixIncludeDir buildDir),
         "-o",
         toFilePath outfile
       ]
    <> ( if
           | not (o ^. compilePreprocess || o ^. compileAssembly) ->
               [ "-L",
                 toFilePath buildDir
               ]
           | otherwise -> []
       )

optimizationOption :: CompileOptions -> String
optimizationOption o =
  "-O" <> show (maybe defaultOptLevel (max 1) (o ^. compileOptimizationLevel))
  where
    defaultOptLevel :: Int
    defaultOptLevel
      | o ^. compileDebug = debugClangOptimizationLevel
      | otherwise = defaultClangOptimizationLevel

native64Args :: Path Abs Dir -> CompileOptions -> Path Abs File -> Path Abs File -> [String]
native64Args buildDir o outfile inputFile =
  commonArgs buildDir o outfile
    <> [ "-DARCH_NATIVE64",
         "-DAPI_LIBC",
         "-m64",
         optimizationOption o,
         toFilePath inputFile
       ]
    <> ( if
           | not (o ^. compilePreprocess || o ^. compileAssembly) ->
               ["-ljuvix"]
           | otherwise -> []
       )

wasiArgs :: Path Abs Dir -> CompileOptions -> Path Abs File -> Path Abs File -> Path Abs Dir -> [String]
wasiArgs buildDir o outfile inputFile sysrootPath =
  commonArgs buildDir o outfile
    <> [ "-DARCH_WASM32",
         "-DAPI_WASI",
         optimizationOption o,
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

findClangOnPath :: (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangOnPath = findExecutable $(mkRelFile "clang")

findClangUsingEnvVar :: forall r. (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangUsingEnvVar = do
  p <- clangBinPath
  join <$> mapM checkExecutable p
  where
    checkExecutable :: Path Abs File -> Sem r (Maybe (Path Abs File))
    checkExecutable p = whenMaybeM (liftIO (isExecutable p)) (return p)

    clangBinPath :: Sem r (Maybe (Path Abs File))
    clangBinPath = fmap (<//> $(mkRelFile "bin/clang")) <$> llvmDistPath

    llvmDistPath :: Sem r (Maybe (Path Abs Dir))
    llvmDistPath = liftIO $ do
      p <- lookupEnv llvmDistEnvironmentVar
      mapM parseAbsDir p

data ClangPath
  = ClangSystemPath (Path Abs File)
  | ClangEnvVarPath (Path Abs File)

extractClangPath :: ClangPath -> Path Abs File
extractClangPath = \case
  ClangSystemPath p -> p
  ClangEnvVarPath p -> p

--- Try searching clang JUVIX_LLVM_DIST_PATH. Otherwise use the PATH
findClang :: (Member EmbedIO r) => Sem r (Maybe ClangPath)
findClang = do
  envVarPath <- findClangUsingEnvVar
  case envVarPath of
    Just p -> return (Just (ClangEnvVarPath p))
    Nothing -> (fmap . fmap) ClangSystemPath findClangOnPath

runClang ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  [String] ->
  Sem r ()
runClang args = do
  cp <- clangBinPath
  (exitCode, _, err) <- liftIO (P.readProcessWithExitCode cp args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> exitFailMsg (pack err)
  where
    clangBinPath :: Sem r String
    clangBinPath = do
      p <- findClang
      maybe (exitFailMsg clangNotFoundErr) (return . toFilePath . extractClangPath) p

    clangNotFoundErr :: Text
    clangNotFoundErr = "Error: The clang executable was not found. Please install the LLVM toolchain"

debugClangOptimizationLevel :: Int
debugClangOptimizationLevel = 1

defaultClangOptimizationLevel :: Int
defaultClangOptimizationLevel = 1

llvmDistEnvironmentVar :: String
llvmDistEnvironmentVar = "JUVIX_LLVM_DIST_PATH"
