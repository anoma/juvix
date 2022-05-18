module MiniJuvix.Utils.Version
  ( branch,
    commit,
    commitDate,
    infoVersionRepo,
    progName,
    progNameVersion,
    progNameVersionTag,
    runDisplayVersion,
    shortHash,
    versionDoc,
    versionTag,
  )
where

import Data.Version (showVersion)
import Development.GitRev (gitBranch, gitCommitDate, gitHash)
import MiniJuvix.Prelude hiding (Doc)
import Paths_minijuvix qualified
import Prettyprinter as PP
import Prettyprinter.Render.Text (renderIO)
import System.Environment (getProgName)

versionDoc :: Doc Text
versionDoc = PP.pretty (showVersion Paths_minijuvix.version)

branch :: Doc Text
branch = PP.pretty (pack $(gitBranch))

commit :: Doc Text
commit = PP.pretty (pack $(gitHash))

commitDate :: Doc Text
commitDate = PP.pretty (pack $(gitCommitDate))

shortHash :: Doc Text
shortHash = PP.pretty (pack (take 7 $(gitHash)))

versionTag :: Doc Text
versionTag = versionDoc <> "-" <> shortHash

progName :: IO (Doc Text)
progName = PP.pretty . pack . toUpperFirst <$> getProgName

progNameVersion :: IO (Doc Text)
progNameVersion = do
  pName <- progName
  return (pName <+> "version" <+> versionDoc)

progNameVersionTag :: IO (Doc Text)
progNameVersionTag = do
  progNameV <- progNameVersion
  return (progNameV <> "-" <> shortHash)

infoVersionRepo :: IO (Doc Text)
infoVersionRepo = do
  pNameTag <- progNameVersionTag
  return
    ( pNameTag <> line
        <> "Branch"
        <> colon <+> branch
        <> line
        <> "Commit"
        <> colon <+> commit
        <> line
        <> "Date"
        <> colon <+> commitDate
        <> line
    )

runDisplayVersion :: IO ()
runDisplayVersion = do
  v <- layoutPretty defaultLayoutOptions <$> infoVersionRepo
  renderIO stdout v
