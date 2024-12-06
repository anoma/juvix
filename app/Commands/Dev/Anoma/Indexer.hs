module Commands.Dev.Anoma.Indexer where

import Anoma.Effect.Base
import Anoma.Effect.Indexer.ListUnrevealedCommits
import Commands.Base
import Commands.Dev.Anoma.Indexer.ListUnrevealedCommits.Options
import Commands.Dev.Anoma.Indexer.Options
import Data.Text qualified as T
import Juvix.Compiler.Nockma.Pretty hiding (Path)

runCommand :: forall r. (Members (Anoma ': Error SimpleError ': AppEffects) r) => AnomaIndexerCommand -> Sem r ()
runCommand = \case
  AnomaIndexerListUnrevealedCommits opts -> do
    res <- listUnrevealedCommits
    case opts ^. indexerListUnrevealedCommitsOutputFile of
      Just out -> do
        f <- fromAppFile out
        let cs = T.unlines (ppPrint <$> res ^. listUnrevealedCommitsResultCommits)
        writeFileEnsureLn' f cs
      Nothing -> do
        forM_ (res ^. listUnrevealedCommitsResultCommits) (renderStdOutLn . ppOutDefault)
