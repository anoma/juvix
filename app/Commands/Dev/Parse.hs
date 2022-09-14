module Commands.Dev.Parse where

import Commands.Base
import Commands.Dev.Parse.Options
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Text.Show.Pretty (ppShow)

runCommand :: Members '[Embed IO, App] r => ParseOptions -> Sem r ()
runCommand opts = do
  m <-
    head . (^. Parser.resultModules)
      <$> runPipeline (opts ^. parseInputFile) upToParsing
  if opts ^. parseNoPrettyShow then say (show m) else say (pack (ppShow m))
