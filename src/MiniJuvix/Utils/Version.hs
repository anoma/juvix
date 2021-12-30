module MiniJuvix.Utils.Version (getVersion) where

------------------------------------------------------------------------------

import Control.Exception (IOException, try)
import qualified Data.List as List
import Data.Version (Version (versionTags))
import MiniJuvix.Utils.Prelude
import System.Exit
import System.Process (readProcessWithExitCode)

------------------------------------------------------------------------------

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

commitInfo :: IO (Maybe String)
commitInfo = do
  res <-
    tryIO $
      readProcessWithExitCode "git" ["log", "--format=%h", "-n", "1"] ""
  case res of
    Right (ExitSuccess, hash, _) -> do
      (_, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
      return $ Just (List.init hash)
    _ -> return Nothing

gitVersion :: Version -> String -> Version
gitVersion version hash = version {versionTags = [take 7 hash]}

-- | If inside a `git` repository, then @'getVersion' x@ will return
-- @x@ plus the hash of the top commit used for
-- compilation. Otherwise, only @x@ will be returned.
getVersion :: Version -> IO Version
getVersion version = do
  commit <- commitInfo
  case commit of
    Nothing -> return version
    Just rev -> return $ gitVersion version rev
