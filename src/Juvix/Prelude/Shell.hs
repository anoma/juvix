module Juvix.Prelude.Shell where

import Juvix.Prelude.Base
import System.Process

echoCmd :: FilePath
echoCmd = "echo"

-- | Rudimentary expansion of environment variables and ~
shellExpandCwd :: String -> IO String
shellExpandCwd arg = readCreateProcess p ""
  where
    p :: CreateProcess
    p = shell cmdString
    cmdString :: String
    cmdString = echoCmd ++ " -n " ++ arg
