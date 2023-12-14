module Commands.Dev.Parse where

import Commands.Base
import Commands.Dev.Parse.Options
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Text.Show.Pretty (ppShow)

runCommand :: (Members '[Embed IO, App, TaggedLock] r) => ParseOptions -> Sem r ()
runCommand opts = do
  m <-
    (^. Parser.resultModule)
      <$> runPipeline (opts ^. parseOptionsInputFile) upToParsing
  if opts ^. parseOptionsNoPrettyShow then say (show m) else say (pack (ppShow m))
