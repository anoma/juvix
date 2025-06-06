module Juvix.Extra.Version where

import Data.Version (Version, showVersion)
import GitHash
import Juvix.Prelude.Base hiding (Doc)
import Juvix.Prelude.Path
import Paths_juvix qualified
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderIO)
import System.Environment qualified as E

versionDir :: Path Rel Dir
versionDir = relDir (unpack fullVersionDoc)

gitInfo :: Maybe GitInfo
gitInfo = eitherToMaybe $$tGitInfoCwdTry

projectOrUnknown :: (GitInfo -> String) -> Text
projectOrUnknown p = maybe "UNKNOWN" (pack . p) gitInfo

version :: Version
version = Paths_juvix.version

-- | Numeric version: x.y.z
numericVersionDoc :: Text
numericVersionDoc = pack (showVersion version)

-- | Numeric version plus the commit
fullVersionDoc :: Text
fullVersionDoc = pack (showVersion version) <> "-" <> commit

branch :: Text
branch = projectOrUnknown giBranch

commit :: Text
commit = projectOrUnknown giHash

commitDate :: Text
commitDate = projectOrUnknown giCommitDate

progName :: (MonadIO m) => m Text
progName = pack . toUpperFirst <$> liftIO E.getProgName

progNameNumericVersion :: (MonadIO m) => m Text
progNameNumericVersion = do
  pName <- progName
  return (pName <> " version " <> numericVersionDoc)

progNamePreciseVersion :: (MonadIO m) => m Text
progNamePreciseVersion = do
  progNameV <- progNameNumericVersion
  return (progNameV <> "-" <> commit)

infoVersionRepo :: (MonadIO m) => m (Doc a)
infoVersionRepo = do
  pNameTag <- progNamePreciseVersion
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
runDisplayNumericVersion = putStrLn numericVersionDoc

runDisplayFullVersion :: (MonadIO m) => m ()
runDisplayFullVersion = putStrLn fullVersionDoc
