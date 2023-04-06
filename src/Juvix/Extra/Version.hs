module Juvix.Extra.Version where

import Data.Version (showVersion)
import Development.GitRev (gitBranch, gitCommitDate, gitHash)
import Juvix.Prelude.Base hiding (Doc)
import Juvix.Prelude.Path
import Paths_juvix qualified
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderIO)
import System.Environment (getProgName)

versionDir :: Path Rel Dir
versionDir = relDir (unpack versionDoc)

versionDoc :: Text
versionDoc = pack (showVersion Paths_juvix.version)

branch :: Text
branch = pack $(gitBranch)

commit :: Text
commit = pack $(gitHash)

commitDate :: Text
commitDate = pack $(gitCommitDate)

shortHash :: Text
shortHash = pack (take 7 $(gitHash))

versionTag :: Text
versionTag = versionDoc <> "-" <> shortHash

progName :: IO Text
progName = pack . toUpperFirst <$> getProgName

progNameVersion :: IO Text
progNameVersion = do
  pName <- progName
  return (pName <> " version " <> versionDoc)

progNameVersionTag :: IO Text
progNameVersionTag = do
  progNameV <- progNameVersion
  return (progNameV <> "-" <> shortHash)

infoVersionRepo :: IO (Doc a)
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

runDisplayVersion :: IO ()
runDisplayVersion = do
  v <- layoutPretty defaultLayoutOptions <$> infoVersionRepo
  renderIO stdout v

runDisplayNumericVersion :: IO ()
runDisplayNumericVersion = putStrLn versionDoc
