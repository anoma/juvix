module Commands.Extra.Compile where

import Commands.Base
import Commands.Extra.Compile.Options
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Extra.Paths
import System.Environment
import System.Process qualified as P

runCommand :: forall r. (Members '[Embed IO, App] r) => CompileOptions -> Sem r ()
runCommand opts = do
  inputFile <- getMainFile (opts ^. compileInputFile)
  result <- runCompile inputFile opts
  case result of
    Left err -> printFailureExit err
    _ -> return ()

runCompile ::
  (Members '[App, Embed IO] r) =>
  Path Abs File ->
  CompileOptions ->
  Sem r (Either Text ())
runCompile inputFile o = do
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  prepareRuntime buildDir o
  case o ^. compileTarget of
    TargetWasm32Wasi -> runError (clangWasmWasiCompile inputFile o)
    TargetNative64 -> runError (clangNativeCompile inputFile o)
    TargetGeb -> return (Right ())
    TargetVampIR -> return (Right ())
    TargetCore -> return (Right ())
    TargetAsm -> return (Right ())
    TargetReg -> return (Right ())
    TargetTree -> return (Right ())
    TargetNockma -> return (Right ())

prepareRuntime :: forall r. (Members '[App, Embed IO] r) => Path Abs Dir -> CompileOptions -> Sem r ()
prepareRuntime buildDir o = do
  mapM_ writeHeader headersDir
  case o ^. compileTarget of
    TargetWasm32Wasi
      | o ^. compileDebug -> writeRuntime wasiDebugRuntime
    TargetWasm32Wasi -> writeRuntime wasiReleaseRuntime
    TargetNative64
      | o ^. compileDebug -> writeRuntime nativeDebugRuntime
    TargetNative64 -> writeRuntime nativeReleaseRuntime
    TargetGeb -> return ()
    TargetVampIR -> return ()
    TargetCore -> return ()
    TargetAsm -> return ()
    TargetReg -> return ()
    TargetTree -> return ()
    TargetNockma -> return ()
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
        . BS.writeFile (toFilePath (buildDir <//> $(mkRelFile "libjuvix.a")))

    headersDir :: [(Path Rel File, BS.ByteString)]
    headersDir = map (first relFile) $(FE.makeRelativeToProject "runtime/include" >>= FE.embedDir)

    includeDir :: Path Abs Dir
    includeDir = juvixIncludeDir buildDir

    writeHeader :: (Path Rel File, BS.ByteString) -> Sem r ()
    writeHeader (filePath, contents) = embed $ do
      ensureDir (includeDir <//> parent filePath)
      BS.writeFile (toFilePath (includeDir <//> filePath)) contents

outputFile :: forall r. (Member App r) => CompileOptions -> Path Abs File -> Sem r (Path Abs File)
outputFile opts inputFile =
  maybe defaultOutputFile fromAppPathFile (opts ^? compileOutputFile . _Just)
  where
    defaultOutputFile :: Sem r (Path Abs File)
    defaultOutputFile = do
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ case opts ^. compileTarget of
        TargetNative64
          | opts ^. compileCOutput -> replaceExtension' cFileExt inputFile
          | opts ^. compilePreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
          | opts ^. compileAssembly -> replaceExtension' ".s" inputFile
          | otherwise -> removeExtension' baseOutputFile
        TargetWasm32Wasi
          | opts ^. compileCOutput -> replaceExtension' cFileExt inputFile
          | opts ^. compilePreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
          | opts ^. compileAssembly -> replaceExtension' ".wat" inputFile
          | otherwise -> replaceExtension' ".wasm" baseOutputFile
        TargetGeb
          | opts ^. compileTerm -> replaceExtension' juvixGebFileExt inputFile
          | otherwise -> replaceExtension' lispFileExt baseOutputFile
        TargetVampIR ->
          replaceExtension' vampIRFileExt baseOutputFile
        TargetCore ->
          replaceExtension' juvixCoreFileExt baseOutputFile
        TargetAsm ->
          replaceExtension' juvixAsmFileExt baseOutputFile
        TargetReg ->
          replaceExtension' juvixRegFileExt baseOutputFile
        TargetTree ->
          replaceExtension' juvixTreeFileExt baseOutputFile
        TargetNockma ->
          replaceExtension' nockmaFileExt baseOutputFile

clangNativeCompile ::
  forall r.
  (Members '[App, Embed IO, Error Text] r) =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangNativeCompile inputFile o = do
  outputFile' <- outputFile o inputFile
  buildDir <- askBuildDir
  if
      | o ^. compileCOutput ->
          copyFile inputFile outputFile'
      | otherwise ->
          runClang (native64Args buildDir o outputFile' inputFile)

clangWasmWasiCompile ::
  forall r.
  (Members '[App, Embed IO, Error Text] r) =>
  Path Abs File ->
  CompileOptions ->
  Sem r ()
clangWasmWasiCompile inputFile o = do
  outputFile' <- outputFile o inputFile
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
        <$> fromMaybeM (throw msg) (embed (lookupEnv "WASI_SYSROOT_PATH"))
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

findClangOnPath :: (Member (Embed IO) r) => Sem r (Maybe (Path Abs File))
findClangOnPath = findExecutable $(mkRelFile "clang")

findClangUsingEnvVar :: forall r. (Member (Embed IO) r) => Sem r (Maybe (Path Abs File))
findClangUsingEnvVar = do
  p <- clangBinPath
  join <$> mapM checkExecutable p
  where
    checkExecutable :: Path Abs File -> Sem r (Maybe (Path Abs File))
    checkExecutable p = whenMaybeM (embed @IO (isExecutable p)) (return p)

    clangBinPath :: Sem r (Maybe (Path Abs File))
    clangBinPath = fmap (<//> $(mkRelFile "bin/clang")) <$> llvmDistPath

    llvmDistPath :: Sem r (Maybe (Path Abs Dir))
    llvmDistPath = do
      p <- embed (lookupEnv llvmDistEnvironmentVar)
      embed @IO (mapM parseAbsDir p)

data ClangPath
  = ClangSystemPath (Path Abs File)
  | ClangEnvVarPath (Path Abs File)

extractClangPath :: ClangPath -> Path Abs File
extractClangPath = \case
  ClangSystemPath p -> p
  ClangEnvVarPath p -> p

--- Try searching clang JUVIX_LLVM_DIST_PATH. Otherwise use the PATH
findClang :: (Member (Embed IO) r) => Sem r (Maybe ClangPath)
findClang = do
  envVarPath <- findClangUsingEnvVar
  case envVarPath of
    Just p -> return (Just (ClangEnvVarPath p))
    Nothing -> (fmap . fmap) ClangSystemPath findClangOnPath

runClang ::
  forall r.
  (Members '[Embed IO, Error Text] r) =>
  [String] ->
  Sem r ()
runClang args = do
  cp <- clangBinPath
  (exitCode, _, err) <- embed (P.readProcessWithExitCode cp args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> throw (pack err)
  where
    clangBinPath :: Sem r String
    clangBinPath = do
      p <- findClang
      maybe (throw clangNotFoundErr) (return . toFilePath . extractClangPath) p

    clangNotFoundErr :: Text
    clangNotFoundErr = "Error: The clang executable was not found. Please install the LLVM toolchain"

debugClangOptimizationLevel :: Int
debugClangOptimizationLevel = 1

defaultClangOptimizationLevel :: Int
defaultClangOptimizationLevel = 1

llvmDistEnvironmentVar :: String
llvmDistEnvironmentVar = "JUVIX_LLVM_DIST_PATH"
