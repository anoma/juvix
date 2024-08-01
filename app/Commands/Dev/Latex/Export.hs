module Commands.Dev.Latex.Export
  ( module Commands.Dev.Latex.Export,
    module Commands.Dev.Latex.Export.Options,
  )
where

import Commands.Base
import Commands.Dev.Latex.Export.Options
import Data.Text qualified as Text
import Juvix.Compiler.Backend.Latex.Translation.FromScoped.Source
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: (Members AppEffects r) => ExportOptions -> Sem r ()
runCommand ExportOptions {..} = do
  res :: Scoper.ScoperResult <- silenceProgressLog (runPipelineNoOptions (Just _exportInputFile) upToScopingEntry)
  let m :: Module 'Scoped 'ModuleTop = res ^. Scoper.resultModule
      c :: Maybe Comments = guard (not _exportNoComments) $> Scoper.getScoperResultComments res
      ltx :: Text =
        Text.unlines
          . sublist (pred <$> _exportFromLine) (pred <$> _exportToLine)
          . Text.lines
          $ moduleToLatex c m
  renderStdOut ltx

sublist :: Maybe Int -> Maybe Int -> [a] -> [a]
sublist mfromIx mtoIx l =
  take
    (toIx + 1 - fromIx)
    (drop fromIx l)
  where
    fromIx = fromMaybe 0 mfromIx
    toIx = fromMaybe (length l - 1) mtoIx
