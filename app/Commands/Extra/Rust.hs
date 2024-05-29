module Commands.Extra.Rust where

import Commands.Base
import System.Process qualified as P

data RustArgs = RustArgs
  { _rustDebug :: Bool,
    _rustInputFile :: Path Abs File,
    _rustOutputFile :: Path Abs File,
    _rustOptimizationLevel :: Maybe Int
  }

makeLenses ''RustArgs

rustCompile ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  RustArgs ->
  Sem r ()
rustCompile args = do
  getRustcCliArgs args >>= runRustc

getRustcCliArgs :: (Members '[App, EmbedIO] r) => RustArgs -> Sem r [String]
getRustcCliArgs args = do
  buildDir <- askBuildDir
  return (nativeArgs buildDir args)

nativeArgs :: Path Abs Dir -> RustArgs -> [String]
nativeArgs buildDir args@RustArgs {..} =
  commonArgs buildDir args ++ extraArgs
  where
    extraArgs :: [String]
    extraArgs = run . execAccumList $ do
      addOptimizationOption args
      addArg (toFilePath _rustInputFile)

addOptimizationOption :: (Member (Accum String) r) => RustArgs -> Sem r ()
addOptimizationOption args = do
  addArg "-C"
  addArg $ "opt-level=" <> show (maybe defaultOptLevel (max 1) (args ^. rustOptimizationLevel))
  where
    defaultOptLevel :: Int
    defaultOptLevel
      | args ^. rustDebug = debugRustOptimizationLevel
      | otherwise = defaultRustOptimizationLevel

debugRustOptimizationLevel :: Int
debugRustOptimizationLevel = 1

defaultRustOptimizationLevel :: Int
defaultRustOptimizationLevel = 3

addArg :: (Member (Accum String) r) => String -> Sem r ()
addArg = accum

commonArgs :: Path Abs Dir -> RustArgs -> [String]
commonArgs buildDir RustArgs {..} = run . execAccumList $ do
  when _rustDebug $ do
    addArg "-g"
    addArg "-C"
    addArg "debug-assertions=true"
    addArg "-C"
    addArg "overflow-checks=true"
  addArg "-o"
  addArg (toFilePath _rustOutputFile)
  addArg ("-L")
  addArg (toFilePath buildDir)

runRustc ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  [String] ->
  Sem r ()
runRustc args = do
  cp <- rustcBinPath
  (exitCode, _, err) <- liftIO (P.readProcessWithExitCode cp args "")
  case exitCode of
    ExitSuccess -> return ()
    _ -> exitFailMsg (pack err)
  where
    rustcBinPath :: Sem r String
    rustcBinPath = do
      p <- findExecutable $(mkRelFile "rustc")
      maybe (exitFailMsg rustcNotFoundErr) (return . toFilePath) p

    rustcNotFoundErr :: Text
    rustcNotFoundErr = "Error: The rustc executable was not found. Please install the Rust compiler"
