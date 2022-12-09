module Main (main) where

import App
import CommonOptions
import Juvix.Compiler.Pipeline
import Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  (global, cli) <- customExecParser p descr
  (root, pkg) <- findRoot' cli
  runM (runAppIO global root pkg (runTopCommand cli))
  where
    findRoot' :: TopCommand -> IO (Path Abs Dir, Package)
    findRoot' cli = findRoot (topCommandInputFile cli)
