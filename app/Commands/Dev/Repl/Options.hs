module Commands.Dev.Repl.Options where

import Commands.Repl.Options
import CommonOptions
import Juvix.Compiler.Core.Data.TransformationId (toEvalTransformations)

parseDevRepl :: Parser ReplOptions
parseDevRepl = do
  let _replPrintValues = False
      _replIsDev = True
  _replInputFile <- optional parseInputJuvixFile
  _replTransformations <- do
    ts <- optTransformationIds
    pure $
      if
          | null ts -> toEvalTransformations
          | otherwise -> ts
  _replNoDisambiguate <- optNoDisambiguate
  _replShowDeBruijn <-
    switch
      ( long "show-de-bruijn"
          <> help "Show variable de Bruijn indices"
      )
  _replNoPrelude <-
    switch
      ( long "no-prelude"
          <> help "Do not load the Prelude module on launch"
      )
  pure ReplOptions {..}
