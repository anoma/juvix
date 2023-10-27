module Juvix.Compiler.Pipeline.Root
  ( module Juvix.Compiler.Pipeline.Root,
    module Juvix.Compiler.Pipeline.Root.Base,
  )
where

import Control.Exception qualified as IO
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Root.Base
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

findRootAndChangeDir ::
  Maybe (Path Abs Dir) ->
  Maybe (Path Abs Dir) ->
  Path Abs Dir ->
  IO Roots
findRootAndChangeDir minputFileDir mbuildDir _rootsInvokeDir = do
  whenJust minputFileDir setCurrentDir
  r <- IO.try go
  case r of
    Left (err :: IO.SomeException) -> do
      putStrLn "Something went wrong when looking for the root of the project"
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right roots -> return roots
  where
    possiblePaths :: Path Abs Dir -> [Path Abs Dir]
    possiblePaths p = p : toList (parents p)

    findPackageFile :: IO (Maybe (Path Abs File))
    findPackageFile = do
      cwd <- getCurrentDir
      let findPackageFile' = findFile (possiblePaths cwd)
      yamlFile <- findPackageFile' Paths.juvixYamlFile
      pFile <- findPackageFile' Paths.packageFilePath
      return (pFile <|> yamlFile)

    go :: IO Roots
    go = do
      l <- findPackageFile
      case l of
        Nothing -> do
          _rootsPackage <- readGlobalPackageIO
          _rootsRootDir <- runM (runFilesIO globalRoot)
          let _rootsPackageGlobal = True
              _rootsBuildDir = getBuildDir mbuildDir _rootsRootDir
          return Roots {..}
        Just yamlPath -> do
          let _rootsRootDir = parent yamlPath
              _rootsPackageGlobal = False
              _rootsBuildDir = getBuildDir mbuildDir _rootsRootDir
          _rootsPackage <- readPackageIO _rootsRootDir (CustomBuildDir (Abs _rootsBuildDir))
          return Roots {..}

getBuildDir :: Maybe (Path Abs Dir) -> Path Abs Dir -> Path Abs Dir
getBuildDir mbuildDirOpt pkgDir = case mbuildDirOpt of
  Nothing -> Paths.rootBuildDir pkgDir
  Just p -> p
