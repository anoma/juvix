module Commands.Dev.Parse where

import Commands.Base
import Commands.Dev.Parse.Options
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Text.Show.Pretty (ppShow)

runCommand :: Members '[Embed IO, App] r => EntryPoint -> ParseOptions -> Sem r ()
runCommand entryPoint localOpts = do
  m <-
    head . (^. Parser.resultModules)
      <$> runPipeline (upToParsing entryPoint)
  if localOpts ^. parseNoPrettyShow then say (show m) else say (pack (ppShow m))
