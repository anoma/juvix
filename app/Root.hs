module Root where

import CommonOptions
import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

type RootDir = Path Abs Dir

type BuildDir = Path Abs Dir

findRootAndChangeDir ::
  Maybe (SomeBase File) ->
  GlobalOptions ->
  IO (RootDir, Package, BuildDir)
findRootAndChangeDir minputFile gopts = do
  whenJust minputFile $ \case
    Abs d -> setCurrentDir (parent d)
    Rel d -> setCurrentDir (parent d)
  r <- IO.try go
  case r of
    Left (err :: IO.SomeException) -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right root -> return root
  where
    possiblePaths :: Path Abs Dir -> [Path Abs Dir]
    possiblePaths p = p : toList (parents p)

    go :: IO (RootDir, Package, BuildDir)
    go = do
      cwd <- getCurrentDir
      l <- findFile (possiblePaths cwd) Paths.juvixYamlFile
      case l of
        Nothing -> do
          let buildDir = getBuildDir gopts cwd cwd
          return (cwd, defaultPackage cwd buildDir, buildDir)
        Just yamlPath -> do
          bs <- ByteString.readFile (toFilePath yamlPath)
          let isEmpty = ByteString.null bs
              root = parent yamlPath
              buildDir = getBuildDir gopts cwd root
          pkg <-
            if
                | isEmpty -> return (defaultPackage root buildDir)
                | otherwise -> readPackageIO root buildDir
          return (root, pkg, buildDir)

getBuildDir :: GlobalOptions -> Path Abs Dir -> Path Abs Dir -> Path Abs Dir
getBuildDir g invokeDir pkgDir = case g ^. globalBuildDir of
  Nothing -> Paths.rootBuildDir pkgDir
  Just (AppPath p _) -> case p of
    Rel r -> invokeDir <//> r
    Abs a -> a
