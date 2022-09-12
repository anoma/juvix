module Commands.Dev.Core.Read where

import Commands.Base
import Commands.Dev.Core.Read.Options
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. Members '[Embed IO, App] r => CoreReadOptions -> Sem r ()
runCommand opts = do
  s' <- embed (readFile f)
  tab <- getRight (fst <$> mapLeft JuvixError (Core.runParser "" f Core.emptyInfoTable s'))
  let tab' = Core.applyTransformations (opts ^. coreReadTransformations) tab
  renderStdOut (Core.ppOut opts tab')
  where
    f :: FilePath
    f = opts ^. coreReadInputFile . unPath
