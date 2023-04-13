module Commands.Dev.Core.Asm where

import AsmInterpreter
import Commands.Base
import Commands.Dev.Core.Asm.Options
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a CoreAsmOptions) => a -> Sem r ()
runCommand opts = do
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- fromAppPathFile sinputFile
  s' <- embed (readFile $ toFilePath inputFile)
  tab <- getRight (mapLeft JuvixError (Core.runParserMain inputFile Core.emptyInfoTable s'))
  r <- runReader (project @GlobalOptions @Core.CoreOptions gopts) $ runError @JuvixError $ Core.toStripped' tab
  tab' <- Asm.fromCore . Stripped.fromCore <$> getRight r
  if
      | project opts ^. coreAsmPrint ->
          renderStdOut (Asm.ppOutDefault tab' tab')
      | otherwise -> runAsm True tab'
  where
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreAsmInputFile
