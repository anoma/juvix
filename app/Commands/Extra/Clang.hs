module Commands.Extra.Clang
  ( module Commands.Extra.Clang,
    module Commands.Extra.Clang.Backend,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Extra.Clang.Backend
import Juvix.Extra.Clang
import Juvix.Extra.Paths
import Juvix.Prelude.Env
import System.Process qualified as P

data ClangArgs = ClangArgs
  { _clangDebug :: Bool,
    _clangInputFile :: Path Abs File,
    _clangOutputFile :: Path Abs File,
    _clangOptimizationLevel :: Maybe Int,
    _clangCStage :: CStage,
    _clangBackend :: ClangBackend
  }

makeLenses ''ClangArgs

clangCompile ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  ClangArgs ->
  Sem r ()
clangCompile args@ClangArgs {..} = do
  case _clangCStage of
    CSource -> copyFile _clangInputFile _clangOutputFile
    _ -> getClangCliArgs args >>= runClang

getClangCliArgs :: (Members '[App, EmbedIO] r) => ClangArgs -> Sem r [String]
getClangCliArgs args@ClangArgs {..} = do
  buildDir <- askBuildDir
  case _clangBackend of
    ClangNative -> return (native64Args buildDir args)
    ClangWasi -> do
      sysroot <- getWasiSysrootPath
      return (wasiArgs buildDir sysroot args)

wasiArgs :: Path Abs Dir -> Path Abs Dir -> ClangArgs -> [String]
wasiArgs buildDir sysrootPath args@ClangArgs {..} =
  commonArgs buildDir args ++ extraArgs
  where
    extraArgs :: [String]
    extraArgs = run . execAccumList $ do
      addArg "-DARCH_WASM32"
      addArg "-DAPI_WASI"
      addArg (optimizationOption args)
      addArg "-nodefaultlibs"
      addArg "--target=wasm32-wasi"
      addArg "--sysroot"
      addArg (toFilePath sysrootPath)
      addArg (toFilePath _clangInputFile)
      when (_clangCStage == CExecutable) (addArg "-ljuvix")

native64Args :: Path Abs Dir -> ClangArgs -> [String]
native64Args buildDir args@ClangArgs {..} =
  commonArgs buildDir args ++ extraArgs
  where
    extraArgs :: [String]
    extraArgs = run . execAccumList $ do
      addArg "-DARCH_NATIVE64"
      addArg "-DAPI_LIBC"
      addArg "-m64"
      addArg (optimizationOption args)
      addArg (toFilePath _clangInputFile)
      when (_clangCStage == CExecutable) (addArg "-ljuvix")

optimizationOption :: ClangArgs -> String
optimizationOption args =
  "-O" <> show (maybe defaultOptLevel (max 1) (args ^. clangOptimizationLevel))
  where
    defaultOptLevel :: Int
    defaultOptLevel
      | args ^. clangDebug = debugClangOptimizationLevel
      | otherwise = defaultClangOptimizationLevel

debugClangOptimizationLevel :: Int
debugClangOptimizationLevel = 1

defaultClangOptimizationLevel :: Int
defaultClangOptimizationLevel = 1

addArg :: (Member (Accum String) r) => String -> Sem r ()
addArg = accum

commonArgs :: Path Abs Dir -> ClangArgs -> [String]
commonArgs buildDir ClangArgs {..} = run . execAccumList $ do
  case _clangCStage of
    CSource -> impossible
    CPreprocess -> addArg "-E"
    CAssembly -> addArg "-S"
    CExecutable -> return ()
  addArg
    $ if
      | _clangDebug -> "-DDEBUG"
      | otherwise -> "-DNDEBUG"
  addArg "-W"
  addArg "-Wall"
  addArg "-Wno-unused-parameter"
  addArg "-Wno-unused-label"
  addArg "-Werror"
  addArg "-std=c11"
  addArg "-I"
  addArg (toFilePath (juvixIncludeDir buildDir))
  addArg "-o"
  addArg (toFilePath _clangOutputFile)
  when
    (_clangCStage == CExecutable)
    $ do
      addArg ("-L")
      addArg (toFilePath buildDir)

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
