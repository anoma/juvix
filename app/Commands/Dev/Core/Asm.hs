module Commands.Dev.Core.Asm where

import Commands.Base
import Commands.Dev.Core.Asm.Options
import AsmInterpreter
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Asm.Translation.FromCore qualified as Asm
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped

runCommand :: forall r a. (Members '[Embed IO, App] r, CanonicalProjection a CoreAsmOptions) => a -> Sem r ()
runCommand opts = do
  inputFile :: Path Abs File <- someBaseToAbs' sinputFile
  s' <- embed (readFile $ toFilePath inputFile)
  (tab, _) <- getRight (mapLeft JuvixError (Core.runParser (toFilePath inputFile) Core.emptyInfoTable s'))
  let tab' = Asm.fromCore $ Stripped.fromCore (Core.toStripped tab)
  if
    | project opts ^. coreAsmPrint ->
      renderStdOut (Asm.ppOutDefault tab' tab')
    | otherwise -> runAsm True tab'
  where
    sinputFile :: SomeBase File
    sinputFile = project opts ^. coreAsmInputFile . pathPath
