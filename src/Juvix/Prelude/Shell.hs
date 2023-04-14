module Juvix.Prelude.Shell where

import Data.Text (stripEnd)
import Juvix.Prelude.Base
import System.Process

echoCmd :: FilePath
echoCmd = "echo"

-- | Rudimentary expansion of environment variables and ~
shellExpandCwd :: String -> IO String
shellExpandCwd arg = rmTrailingNewline <$> readCreateProcess p ""
  where
    rmTrailingNewline :: String -> String
    rmTrailingNewline = unpack . stripEnd . pack
    p :: CreateProcess
    p = shell cmdString
    cmdString :: String
    cmdString = echoCmd ++ " " ++ arg
