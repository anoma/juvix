module Root where

import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

findRoot :: Maybe (SomeBase File) -> IO (Path Abs Dir, Package)
findRoot minputFile = do
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
    possiblePaths = parents

    go :: IO (Path Abs Dir, Package)
    go = do
      c <- getCurrentDir
      l <- findFile (possiblePaths c) Paths.juvixYamlFile
      case l of
        Nothing -> return (c, emptyPackage)
        Just yamlPath -> do
          bs <- ByteString.readFile (toFilePath yamlPath)
          let isEmpty = ByteString.null bs
              root = parent yamlPath
          pkg <-
            if
                | isEmpty -> return emptyPackage
                | otherwise -> readPackageIO root
          return (root, pkg)
