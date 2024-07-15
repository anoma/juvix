module Commands.Dev.Core.Asm where

import AsmInterpreter
import Commands.Base
import Commands.Dev.Core.Asm.Options
import Juvix.Compiler.Asm qualified as Asm
import Juvix.Compiler.Core qualified as Core

runCommand ::
  forall r a.
  (Members '[EmbedIO, App, TaggedLock] r, CanonicalProjection a CoreAsmOptions) =>
  a ->
  Sem r ()
runCommand opts = do
  inputFile :: Path Abs File <- getMainFile (Just sinputFile)
  ep <- getEntryPoint (Just sinputFile)
  s' <- readFile inputFile
  tab <- getRight (Core.runParserMain inputFile defaultModuleId mempty s')
  r <- runReader ep . runError @JuvixError $ coreToAsm (Core.moduleFromInfoTable tab)
  tab' <- getRight r
  if
    | project opts ^. coreAsmPrint ->
        renderStdOut (Asm.ppOutDefault tab' tab')
    | otherwise -> runAsm True tab'
  where
    sinputFile :: AppPath File
    sinputFile = project opts ^. coreAsmInputFile
