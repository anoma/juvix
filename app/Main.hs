module Main (main) where

import App
import CommonOptions
import Control.Exception qualified as IO
import Data.ByteString qualified as ByteString
import Data.Yaml
import Juvix.Compiler.Pipeline
import Juvix.Extra.Paths qualified as Paths
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  (global, cli) <- customExecParser p descr >>= secondM makeAbsPaths
  (root, pkg) <- findRoot cli
  runM (runAppIO global root pkg (runTopCommand cli))

findRoot :: TopCommand -> IO (FilePath, Package)
findRoot cli = do
  let dir :: Maybe FilePath
      dir = takeDirectory <$> topCommandInputFile cli
  whenJust dir setCurrentDirectory
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
        Just yaml -> do
          bs <- ByteString.readFile yaml
          let isEmpty = ByteString.null bs
          pkg <-
            if
                | isEmpty -> return emptyPackage
                | otherwise -> decodeThrow bs
          return (takeDirectory yaml, pkg)
