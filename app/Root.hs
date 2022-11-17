module Root where

import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import Juvix.Prelude

findRoot :: Maybe FilePath -> IO (FilePath, Package)
findRoot minputFile = do
  whenJust (takeDirectory <$> minputFile) setCurrentDirectory
  r <- IO.try go
  case r of
    Left (err :: IO.SomeException) -> do
      putStrLn "Something went wrong when figuring out the root of the project."
      putStrLn (pack (IO.displayException err))
      exitFailure
    Right root -> return root
  where
    possiblePaths :: FilePath -> [FilePath]
    possiblePaths start = takeWhile (/= "/") (aux start)
      where
        aux f = f : aux (takeDirectory f)

    go :: IO (FilePath, Package)
    go = do
      c <- getCurrentDirectory
      l <- findFile (possiblePaths c) Paths.juvixYamlFile
      case l of
        Nothing -> return (c, emptyPackage)
        Just yamlPath -> do
          bs <- ByteString.readFile yamlPath
          let isEmpty = ByteString.null bs
              root = takeDirectory yamlPath
          pkg <-
            if
                | isEmpty -> return emptyPackage
                | otherwise -> readPackageIO root
          return (root, pkg)
