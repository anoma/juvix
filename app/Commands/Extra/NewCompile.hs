-- TODO find a better module name
module Commands.Extra.NewCompile
  ( module Commands.Extra.NewCompile,
    module Commands.Extra.Clang,
  )
where

import Commands.Base
import Commands.CompileNew.CommonOptions
import Commands.Extra.Clang
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Compiler.Backend
import Juvix.Compiler.Core qualified as Core
import Juvix.Extra.Paths.Base

getOutputFile :: (Members '[App] r) => FileExt -> Maybe (AppPath File) -> Maybe (AppPath File) -> Sem r (Path Abs File)
getOutputFile ext inp mout = do
  case mout of
    Just out -> fromAppPathFile out
    Nothing -> do
      mainFile <- getMainFile inp
      return (replaceExtension' (unpack (fileExtToText ext)) mainFile)

compileToCore :: (Members '[App, EmbedIO, TaggedLock] r) => CompileCommonOptions -> Sem r Core.CoreResult
compileToCore opts = runPipeline (Just (opts ^. compileInputFile)) upToCore

runCommandMain ::
  forall r.
  (Members '[EmbedIO, Reader Target, App] r) =>
  CompileCommonOptionsMain ->
  Sem r ()
runCommandMain = fromCompileCommonOptionsMain >=> runCommand

runCommand ::
  forall r.
  (Members '[EmbedIO, Reader Target, App] r) =>
  CompileCommonOptions ->
  Sem r ()
runCommand = runCompile

-- clangWasmWasiCompile ::
--   forall r.
--   (Members '[App, EmbedIO, Error Text] r) =>
--   CompileCommonOptions ->
--   Sem r ()
-- clangWasmWasiCompile o = do
--   inputFile <- getMainFile (Just (o ^. compileInputFile))
--   outputFile' <- outputFile o
--   buildDir <- askBuildDir
--   if
--       | o ^. compileCOutput ->
--           copyFile inputFile outputFile'
--       | otherwise -> do
--           clangArgs <- wasiArgs buildDir o outputFile' inputFile <$> sysrootEnvVar
--           runClang clangArgs
--   where
--     sysrootEnvVar :: Sem r (Path Abs Dir)
--     sysrootEnvVar =
--       absDir
--         <$> fromMaybeM (throw msg) (liftIO (lookupEnv "WASI_SYSROOT_PATH"))
--       where
--         msg :: Text
--         msg = "Missing environment variable WASI_SYSROOT_PATH"

runCompile ::
  (Members '[App, Reader Target, EmbedIO] r) =>
  CompileCommonOptions ->
  Sem r ()
runCompile opts = do
  buildDir <- askBuildDir
  ensureDir buildDir
  ensureDir (juvixIncludeDir buildDir)
  prepareRuntime buildDir opts
  target <- ask @Target
  case target of {}

-- TargetCWasm32Wasi -> runError (clangWasmWasiCompile opts)
-- TargetNative64 -> runError (clangNativeCompile opts)
-- TargetGeb -> return (Right ())
-- TargetVampIR -> return (Right ())
-- TargetCore -> return (Right ())
-- TargetAsm -> return (Right ())
-- TargetReg -> return (Right ())
-- TargetTree -> return (Right ())
-- TargetAnoma -> return (Right ())
-- TargetCasm -> return (Right ())
-- TargetCairo -> return (Right ())

prepareRuntime ::
  forall r.
  (Members '[App, Reader Target, EmbedIO] r) =>
  Path Abs Dir ->
  CompileCommonOptions ->
  Sem r ()
prepareRuntime buildDir o = do
  mapM_ writeHeader headersDir
  target <- ask @Target
  case target of {}
  where
    -- TargetWasm32Wasi
    --   | o ^. compileDebug -> writeRuntime wasiDebugRuntime
    -- TargetWasm32Wasi -> writeRuntime wasiReleaseRuntime
    -- TargetNative64
    --   | o ^. compileDebug -> writeRuntime nativeDebugRuntime
    -- TargetNative64 -> writeRuntime nativeReleaseRuntime
    -- TargetGeb -> return ()
    -- TargetVampIR -> return ()
    -- TargetCore -> return ()
    -- TargetAsm -> return ()
    -- TargetReg -> return ()
    -- TargetTree -> return ()
    -- TargetAnoma -> return ()
    -- TargetCasm -> return ()
    -- TargetCairo -> return ()

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
      liftIO
        . BS.writeFile (toFilePath (buildDir <//> $(mkRelFile "libjuvix.a")))

    headersDir :: [(Path Rel File, BS.ByteString)]
    headersDir = map (first relFile) $(FE.makeRelativeToProject "runtime/include" >>= FE.embedDir)

    includeDir :: Path Abs Dir
    includeDir = juvixIncludeDir buildDir

    writeHeader :: (Path Rel File, BS.ByteString) -> Sem r ()
    writeHeader (filePath, contents) = liftIO $ do
      ensureDir (includeDir <//> parent filePath)
      BS.writeFile (toFilePath (includeDir <//> filePath)) contents
