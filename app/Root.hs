module Root where

import Juvix.Prelude
import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import Juvix.Compiler.Pipeline.Package
import Juvix.Extra.Paths qualified as Paths

type RootDir = Path Abs Dir

type BuildDir = Path Abs Dir

type IsGlobal = Bool

findRootAndChangeDir ::
  IO (Maybe (SomeBase Dir)) ->
  Maybe (SomeBase Dir) ->
  Path Abs Dir ->
  IO (RootDir, (Package, IsGlobal), BuildDir)
findRootAndChangeDir minputFile mbuildDir invokeDir = do
  whenJustM minputFile $ \case
    Abs d -> setCurrentDir d
    Rel d -> setCurrentDir d
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

    go :: IO (RootDir, (Package, IsGlobal), BuildDir)
    go = do
      cwd <- getCurrentDir
      l <- findFile (possiblePaths cwd) Paths.juvixYamlFile
      let buildDir = getBuildDir mbuildDir invokeDir cwd
      case l of
        Nothing -> do
          globPkg <- readGlobalPackageIO
          root <- runM (runFilesIO globalRoot)
          return (root, (globPkg, True), buildDir)
        Just yamlPath -> do
          bs <- ByteString.readFile (toFilePath yamlPath)
          let isEmpty = ByteString.null bs
              root = parent yamlPath
          pkg <-
            if
                | isEmpty -> return emptyPackage
                | otherwise -> readPackageIO root (Abs buildDir)
          return (root, (pkg, False), buildDir)

getBuildDir :: Maybe (SomeBase Dir) -> Path Abs Dir -> Path Abs Dir -> Path Abs Dir
getBuildDir mbuildDir invokeDir pkgDir = case mbuildDir of
  Nothing -> Paths.rootBuildDir pkgDir
  Just p -> case p of
    Rel r -> invokeDir <//> r
    Abs a -> a
