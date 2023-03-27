module Commands.Dev.Core.Strip where

import Commands.Base
import Commands.Dev.Core.Strip.Options
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a Core.Options, CanonicalProjection a CoreStripOptions) => a -> Sem r ()
runCommand opts = do
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- someBaseToAbs' sinputFile
  s' <- embed (readFile $ toFilePath inputFile)
  (tab, _) <- getRight (mapLeft JuvixError (Core.runParser inputFile Core.emptyInfoTable s'))
  let r =
        run $
          runReader (project gopts) $
            runError @JuvixError (Core.toStripped' tab :: Sem '[Error JuvixError, Reader Core.CoreOptions] Core.InfoTable)
  tab' <- getRight $ mapLeft JuvixError $ mapRight Stripped.fromCore r
  unless (project opts ^. coreStripNoPrint) $ do
    renderStdOut (Core.ppOut opts tab')
  where
    sinputFile :: SomeBase File
    sinputFile = project opts ^. coreStripInputFile . pathPath
