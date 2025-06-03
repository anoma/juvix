module Commands.Dev.DisplayBuildDir where

import Commands.Base
import Commands.Dev.DisplayBuildDir.Options
import Juvix.Extra.Paths.Base qualified as Paths

runCommand :: forall r. (Members AppEffects r) => BuildDirOptions -> Sem r ()
runCommand opts = do
  fp :: String <-
    if
        | opts ^. buildDirRelative -> return (toFilePath Paths.relBuildDir)
        | otherwise -> do
            root <- askPkgDir
            return (toFilePath (Paths.rootBuildDir root))
  renderStdOutLn fp
