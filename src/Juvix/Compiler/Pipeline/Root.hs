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
  IO Root
findRootAndChangeDir minputFileDir mbuildDir _rootInvokeDir = do
  whenJust minputFileDir setCurrentDir
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
      cwd <- getCurrentDir
      let findPackageFile' = findFile (possiblePaths cwd)
      yamlFile <- findPackageFile' Paths.juvixYamlFile
      pFile <- findPackageFile' Paths.packageFilePath
      return (pFile <|> yamlFile)

    go :: IO Root
    go = do
      l <- findPackageFile
      case l of
        Nothing -> do
          _rootPackage <- readGlobalPackageIO
          _rootRootDir <- runM (runFilesIO globalRoot)
          let _rootPackageGlobal = True
              _rootBuildDir = getBuildDir mbuildDir _rootRootDir
          return Root {..}
        Just yamlPath -> do
          let _rootRootDir = parent yamlPath
              _rootPackageGlobal = False
              _rootBuildDir = getBuildDir mbuildDir _rootRootDir
          _rootPackage <- readPackageIO _rootRootDir (CustomBuildDir (Abs _rootBuildDir))
          return Root {..}

getBuildDir :: Maybe (Path Abs Dir) -> Path Abs Dir -> Path Abs Dir
getBuildDir mbuildDirOpt pkgDir = case mbuildDirOpt of
  Nothing -> Paths.rootBuildDir pkgDir
  Just p -> p
