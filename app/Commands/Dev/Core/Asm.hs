module Commands.Dev.Core.Asm where

import AsmInterpreter
import Commands.Base
import Commands.Dev.Core.Asm.Options
import Juvix.Compiler.Asm qualified as Asm
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a CoreAsmOptions) => a -> Sem r ()
runCommand opts = do
  gopts <- askGlobalOptions
  inputFile :: Path Abs File <- fromAppPathFile sinputFile
  s' <- readFile $ toFilePath inputFile
  tab <- getRight (mapLeft JuvixError (Core.runParserMain inputFile defaultModuleId mempty s'))
  r <- runReader (project @GlobalOptions @Core.CoreOptions gopts) $ runError @JuvixError $ Core.toStripped' (Core.moduleFromInfoTable tab)
  tab' <- Asm.fromCore . Stripped.fromCore . Core.computeCombinedInfoTable <$> getRight r
  if
      | project opts ^. coreAsmPrint ->
          renderStdOut (Asm.ppOutDefault tab' tab')
      | otherwise -> runAsm True tab'
  where
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreAsmInputFile
