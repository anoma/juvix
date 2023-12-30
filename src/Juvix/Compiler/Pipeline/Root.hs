module Juvix.Compiler.Pipeline.Root
  ( module Juvix.Compiler.Pipeline.Root,
    module Juvix.Compiler.Pipeline.Root.Base,
  )
where

import Control.Exception (SomeException)
import Control.Exception qualified as IO
import Juvix.Compiler.Pipeline.Package.IO
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

readPackageRootIO :: (Members '[TaggedLock, Embed IO] r) => Root -> Sem r Package
readPackageRootIO root = readPackageIO (root ^. rootRootDir) (root ^. rootBuildDir)

findRootAndChangeDir ::
  forall r.
  (Members '[TaggedLock, Embed IO, Final IO] r) =>
  Maybe (Path Abs Dir) ->
  Maybe (Path Abs Dir) ->
  Path Abs Dir ->
  Sem r Root
findRootAndChangeDir minputFileDir mbuildDir _rootInvokeDir = do
  r <- runError (fromExceptionSem @SomeException go)
  runFilesIO ensureGlobalPackage
  case r of
    Left (err :: IO.SomeException) -> liftIO $ do
      putStrLn "Something went wrong when looking for the root of the project"
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right root -> return root
  where
    possiblePaths :: Path Abs Dir -> [Path Abs Dir]
    possiblePaths p = p : toList (parents p)

    findPackageFile :: (Members '[Embed IO] r') => Sem r' (Maybe (Path Abs File))
    findPackageFile = do
      let cwd = fromMaybe _rootInvokeDir minputFileDir
          findPackageFile' = findFile (possiblePaths cwd)
      yamlFile <- findPackageFile' Paths.juvixYamlFile
      pFile <- findPackageFile' Paths.packageFilePath
      return (pFile <|> yamlFile)

    go :: Sem (Error SomeException ': r) Root
    go = do
      l <- findPackageFile
      case l of
        Nothing -> do
          let cwd = fromMaybe _rootInvokeDir minputFileDir
          packageBaseRootDir <- runFilesIO globalPackageBaseRoot
          (_rootRootDir, _rootPackageType) <-
            if
                | isPathPrefix packageBaseRootDir cwd ->
                    return (packageBaseRootDir, GlobalPackageBase)
                | otherwise -> do
                    r <- runFilesIO globalRoot
                    return (r, GlobalStdlib)
          let _rootBuildDir = getBuildDir mbuildDir
          return Root {..}
        Just pkgPath -> do
          packageDescriptionRootDir <- runFilesIO globalPackageDescriptionRoot
          let _rootRootDir = parent pkgPath
              _rootPackageType
                | isPathPrefix packageDescriptionRootDir _rootRootDir = GlobalPackageDescription
                | otherwise = LocalPackage
              _rootBuildDir = getBuildDir mbuildDir
          return Root {..}

getBuildDir :: Maybe (Path Abs Dir) -> BuildDir
getBuildDir mbuildDirOpt = case mbuildDirOpt of
  Nothing -> DefaultBuildDir
  Just p -> CustomBuildDir (Abs p)
