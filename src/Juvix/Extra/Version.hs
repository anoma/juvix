module Juvix.Extra.Version where

import Data.Version (showVersion)
import GitHash
import Juvix.Prelude.Base hiding (Doc)
import Juvix.Prelude.Path
import Paths_juvix qualified
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderIO)
import System.Environment qualified as E

versionDir :: Path Rel Dir
versionDir = relDir (unpack versionDoc)

gitInfo :: Maybe GitInfo
gitInfo = eitherToMaybe $$tGitInfoCwdTry

projectOrUnknown :: (GitInfo -> String) -> Text
projectOrUnknown p = maybe "UNKNOWN" (pack . p) gitInfo

versionDoc :: Text
versionDoc = pack (showVersion Paths_juvix.version)

branch :: Text
branch = projectOrUnknown giBranch

commit :: Text
commit = projectOrUnknown giHash

commitDate :: Text
commitDate = projectOrUnknown giCommitDate

shortHash :: Text
shortHash = projectOrUnknown (take 7 . giHash)

versionTag :: Text
versionTag = versionDoc <> "-" <> shortHash

progName :: (MonadIO m) => m Text
progName = pack . toUpperFirst <$> liftIO E.getProgName

progNameVersion :: (MonadIO m) => m Text
progNameVersion = do
  pName <- progName
  return (pName <> " version " <> versionDoc)

progNameVersionTag :: (MonadIO m) => m Text
progNameVersionTag = do
  progNameV <- progNameVersion
  return (progNameV <> "-" <> shortHash)

infoVersionRepo :: (MonadIO m) => m (Doc a)
infoVersionRepo = do
  pNameTag <- progNameVersionTag
  return
    ( PP.pretty pNameTag
        <> line
        <> "Branch"
        <> colon
        <+> PP.pretty branch
          <> line
          <> "Commit"
          <> colon
        <+> PP.pretty commit
          <> line
          <> "Date"
          <> colon
        <+> PP.pretty commitDate
          <> line
    )

runDisplayVersion :: (MonadIO m) => m ()
runDisplayVersion = do
  v <- layoutPretty defaultLayoutOptions <$> infoVersionRepo
  liftIO (renderIO stdout v)

runDisplayNumericVersion :: (MonadIO m) => m ()
runDisplayNumericVersion = putStrLn versionDoc
