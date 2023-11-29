module Juvix.Compiler.Pipeline.Root
  ( module Juvix.Compiler.Pipeline.Root,
    module Juvix.Compiler.Pipeline.Root.Base,
  )
where

import Control.Exception qualified as IO
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

findRootAndChangeDir ::
  LockMode ->
  Maybe (Path Abs Dir) ->
  Maybe (Path Abs Dir) ->
  Path Abs Dir ->
  IO Root
findRootAndChangeDir lockMode minputFileDir mbuildDir _rootInvokeDir = do
  r <- IO.try go
  case r of
    Left (err :: IO.SomeException) -> do
      putStrLn "Something went wrong when looking for the root of the project"
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right root -> return root
  where
    possiblePaths :: Path Abs Dir -> [Path Abs Dir]
    possiblePaths p = p : toList (parents p)

    findPackageFile :: IO (Maybe (Path Abs File))
    findPackageFile = do
      let cwd = fromMaybe _rootInvokeDir minputFileDir
          findPackageFile' = findFile (possiblePaths cwd)
      yamlFile <- findPackageFile' Paths.juvixYamlFile
      pFile <- findPackageFile' Paths.packageFilePath
      return (pFile <|> yamlFile)

    go :: IO Root
    go = do
      l <- findPackageFile
      case l of
        Nothing -> do
          let cwd = fromMaybe _rootInvokeDir minputFileDir
          packageBaseRootDir <- runM (runFilesIO globalPackageBaseRoot)
          (_rootPackage, _rootRootDir, _rootPackageGlobal) <-
            if
                | isPathPrefix packageBaseRootDir cwd -> return (packageBasePackage, packageBaseRootDir, False)
                | otherwise -> do
                    globalPkg <- readGlobalPackageIO lockMode
                    r <- runM (runFilesIO globalRoot)
                    return (globalPkg, r, True)
          let _rootBuildDir = getBuildDir mbuildDir
          return Root {..}
        Just yamlPath -> do
          let _rootRootDir = parent yamlPath
              _rootPackageGlobal = False
              _rootBuildDir = getBuildDir mbuildDir
          _rootPackage <- readPackageIO lockMode _rootRootDir _rootBuildDir
          return Root {..}

getBuildDir :: Maybe (Path Abs Dir) -> BuildDir
getBuildDir mbuildDirOpt = case mbuildDirOpt of
  Nothing -> DefaultBuildDir
  Just p -> CustomBuildDir (Abs p)
