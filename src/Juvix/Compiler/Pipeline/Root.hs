module Juvix.Compiler.Pipeline.Root where

import Control.Exception qualified as IO
import Juvix.Compiler.Pipeline.Package
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

data Roots = Roots
  { _rootsRootDir :: Path Abs Dir,
    _rootsPackage :: Package,
    _rootsPackageGlobal :: Bool,
    _rootsBuildDir :: Path Abs Dir,
    _rootsInvokeDir :: Path Abs Dir
  }
  deriving stock (Show)

makeLenses ''Roots

findRootAndChangeDir ::
  Maybe (SomeBase Dir) ->
  Maybe (SomeBase Dir) ->
  Path Abs Dir ->
  IO Roots
findRootAndChangeDir minputFileDir mbuildDir _rootsInvokeDir = do
  whenJust minputFileDir $ \case
    Abs d -> setCurrentDir d
    Rel d -> setCurrentDir d
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

    go :: IO Roots
    go = do
      cwd <- getCurrentDir
      l <- findFile (possiblePaths cwd) Paths.juvixYamlFile
      case l of
        Nothing -> do
          _rootsPackage <- readGlobalPackageIO
          _rootsRootDir <- runM (runFilesIO globalRoot)
          let _rootsPackageGlobal = True
              _rootsBuildDir = getBuildDir mbuildDir _rootsInvokeDir _rootsRootDir
          return Roots {..}
        Just yamlPath -> do
          let _rootsRootDir = parent yamlPath
              _rootsPackageGlobal = False
              _rootsBuildDir = getBuildDir mbuildDir _rootsInvokeDir _rootsRootDir
          _rootsPackage <- readPackageIO _rootsRootDir (Abs _rootsBuildDir)
          return Roots {..}

getBuildDir :: Maybe (SomeBase Dir) -> Path Abs Dir -> Path Abs Dir -> Path Abs Dir
getBuildDir mbuildDirOpt invokeDir pkgDir = case mbuildDirOpt of
  Nothing -> Paths.rootBuildDir pkgDir
  Just p -> case p of
    Rel r -> invokeDir <//> r
    Abs a -> a
