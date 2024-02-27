module Commands.Dev.Core.Strip where

import Commands.Base
import Commands.Dev.Core.Strip.Options
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[EmbedIO, App] r, CanonicalProjection a Core.Options, CanonicalProjection a CoreStripOptions) => a -> Sem r ()
runCommand opts = do
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- fromAppPathFile sinputFile
  s' <- readFile inputFile
  (tab, _) <- getRight (mapLeft JuvixError (Core.runParser inputFile defaultModuleId mempty s'))
  let r =
        run $
          runReader (project gopts) $
            runError @JuvixError (Core.toStripped' Core.Identity (Core.moduleFromInfoTable tab) :: Sem '[Error JuvixError, Reader Core.CoreOptions] Core.Module)
  tab' <- getRight $ mapLeft JuvixError $ mapRight (Stripped.fromCore (project gopts ^. Core.optFieldSize) . Core.computeCombinedInfoTable) r
  unless (project opts ^. coreStripNoPrint) $ do
    renderStdOut (Core.ppOut opts tab')
  where
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreStripInputFile
