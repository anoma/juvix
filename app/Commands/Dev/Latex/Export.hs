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

runCommand :: forall r. (Members AppEffects r) => ExportOptions -> Sem r ()
runCommand ExportOptions {..} = do
  res :: Scoper.ScoperResult <- silenceProgressLog (runPipelineNoOptions (Just _exportInputFile) upToScopingEntry)
  inputAbs :: Path Abs File <- fromAppPathFile _exportInputFile
  let m :: Module 'Scoped 'ModuleTop = res ^. Scoper.resultModule
      c :: Maybe FileComments = do
        guard (not _exportNoComments)
        return (fromJust (Scoper.getScoperResultComments res ^. commentsByFile . at inputAbs))
  ltx :: Text <- case _exportFilter of
    ExportFilterNames names -> goNames m names
    ExportFilterRange ExportRange {..} ->
      return
        . Text.unlines
        . sublist (pred <$> _exportFromLine) (pred <$> _exportToLine)
        . Text.lines
        $ concreteToLatex c m
  renderStdOutLn $
    case _exportMode of
      ExportStandalone -> standalone ltx
      ExportRaw -> ltx
      ExportWrap -> verb ltx
  where
    goNames :: Module 'Scoped 'ModuleTop -> [Text] -> Sem r Text
    goNames m ns = do
      stms :: [Statement 'Scoped] <- mapM getStatement ns
      return (concreteToLatex Nothing (Statements stms))
      where
        getStatement :: Text -> Sem r (Statement 'Scoped)
        getStatement lbl = case tbl ^. at lbl of
          Nothing -> exitFailMsg ("The statement " <> lbl <> " does not exist")
          Just s -> return s

        tbl :: HashMap Text (Statement 'Scoped)
        tbl = topStatementsByLabel m

verb :: Text -> Text
verb code =
  [__i|
  \\begin{tcolorbox}[colback=ju-base, colframe=ju-crust]
  \\begin{Verbatim}[commandchars=\\\\\\{\\}]
  #{code}
  \\end{Verbatim}
  \\end{tcolorbox}
  |]

standalone :: Text -> Text
standalone code =
  [__i|
  \\documentclass{article}
  \\usepackage{tcolorbox}
  \\usepackage{fvextra}
  \\usepackage[theme=latte]{juvix}
  \\begin{document}
  #{verb code}
  \\end{document}
 |]

sublist :: Maybe Int -> Maybe Int -> [a] -> [a]
sublist mfromIx mtoIx l =
  take
    (toIx + 1 - fromIx)
    (drop fromIx l)
  where
    fromIx = fromMaybe 0 mfromIx
    toIx = fromMaybe (length l - 1) mtoIx
