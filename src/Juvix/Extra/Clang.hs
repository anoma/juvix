module Juvix.Extra.Clang where

import Juvix.Config qualified as Config
import Juvix.Prelude
import System.Environment
import System.FilePath

data ClangPath
  = ClangSystemPath (Path Abs File)
  | ClangEnvVarPath (Path Abs File)

-- | If the path specified in Config is absolute, try using it. Otherwise, try
-- searching the relative path to clang in JUVIX_LLVM_DIST_PATH. Otherwise use
-- the PATH.
findClang :: (Member EmbedIO r) => Sem r (Maybe ClangPath)
findClang = do
  mp <- findClangFromConfig
  case mp of
    Just p -> return (Just (ClangSystemPath p))
    Nothing -> do
      envVarPath <- findClangUsingEnvVar
      case envVarPath of
        Just p -> return (Just (ClangEnvVarPath p))
        Nothing -> (fmap . fmap) ClangSystemPath findClangOnPath

checkExecutable :: forall r. (Member EmbedIO r) => Path Abs File -> Sem r (Maybe (Path Abs File))
checkExecutable p = whenMaybeM (liftIO (isExecutable p)) (return p)

findClangFromConfig :: forall r. (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangFromConfig =
  if
      | isAbsolute clangPath -> do
          let p :: Maybe (Path Abs File)
              p = parseAbsFile clangPath
          join <$> mapM checkExecutable p
      | otherwise -> return Nothing
  where
    clangPath :: FilePath
    clangPath = unpack $ Config.config ^. Config.configClang

findClangRelPath :: Maybe (Path Rel File)
findClangRelPath = parseRelFile (unpack $ Config.config ^. Config.configClang)

findClangUsingEnvVar' :: forall r. (Member EmbedIO r) => Path Rel File -> Sem r (Maybe (Path Abs File))
findClangUsingEnvVar' clangRelPath = do
  p <- clangBinPath
  join <$> mapM checkExecutable p
  where
    clangBinPath :: Sem r (Maybe (Path Abs File))
    clangBinPath = fmap (<//> $(mkRelDir "bin") <//> clangRelPath) <$> llvmDistPath

    llvmDistPath :: Sem r (Maybe (Path Abs Dir))
    llvmDistPath = liftIO $ do
      p <- lookupEnv llvmDistEnvironmentVar
      mapM parseAbsDir p

findClangUsingEnvVar :: forall r. (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangUsingEnvVar =
  case findClangRelPath of
    Just clangRelPath -> findClangUsingEnvVar' clangRelPath
    Nothing -> findClangUsingEnvVar' $(mkRelFile "clang")

extractClangPath :: ClangPath -> Path Abs File
extractClangPath = \case
  ClangSystemPath p -> p
  ClangEnvVarPath p -> p

llvmDistEnvironmentVar :: String
llvmDistEnvironmentVar = "JUVIX_LLVM_DIST_PATH"

findClangOnPath :: (Member EmbedIO r) => Sem r (Maybe (Path Abs File))
findClangOnPath =
  case findClangRelPath of
    Just clangRelPath -> findExecutable clangRelPath
    Nothing -> findExecutable $(mkRelFile "clang")
