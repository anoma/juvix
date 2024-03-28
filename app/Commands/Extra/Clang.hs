module Commands.Extra.Clang where

import Commands.Base
import Commands.CompileNew.CStage
import Juvix.Extra.Paths
import System.Environment
import System.Process qualified as P

data ClangArgs = ClangArgs
  { _clangDebug :: Bool,
    _clangInputFile :: Path Abs File,
    _clangOutputFile :: Path Abs File,
    _clangOptimizationLevel :: Maybe Int,
    _clangCStage :: CStage
  }

makeLenses ''ClangArgs

clangNativeCompile ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  ClangArgs ->
  Sem r ()
clangNativeCompile args@ClangArgs {..} = do
  buildDir <- askBuildDir
  case _clangCStage of
    CSource -> copyFile _clangInputFile _clangOutputFile
    _ -> runClang (native64Args buildDir args)

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
  addArg $
    if
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

data ClangPath
  = ClangSystemPath (Path Abs File)
  | ClangEnvVarPath (Path Abs File)

-- | Try searching clang JUVIX_LLVM_DIST_PATH. Otherwise use the PATH
findClang :: (Member EmbedIO r) => Sem r (Maybe ClangPath)
findClang = do
  envVarPath <- findClangUsingEnvVar
  case envVarPath of
    Just p -> return (Just (ClangEnvVarPath p))
    Nothing -> (fmap . fmap) ClangSystemPath findClangOnPath

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

extractClangPath :: ClangPath -> Path Abs File
extractClangPath = \case
  ClangSystemPath p -> p
  ClangEnvVarPath p -> p

llvmDistEnvironmentVar :: String
llvmDistEnvironmentVar = "JUVIX_LLVM_DIST_PATH"

findClangOnPath :: (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangOnPath = findExecutable $(mkRelFile "clang")
