module Commands.Clean where

import Commands.Base

runCommand :: (Members '[Files, App] r) => Sem r ()
runCommand = do
  buildDir <- askBuildDir
  whenM (directoryExists' buildDir) (removeDirectoryRecursive' buildDir)
