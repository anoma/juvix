module Tree.Asm.Base where

import Asm.Run.Base qualified as Asm
import Base
import Juvix.Compiler.Asm.Translation.FromTree qualified as Asm
import Juvix.Compiler.Tree.Pipeline qualified as Tree
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeAsmAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
treeAsmAssertion root' mainFile expectedFile step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (prettyString err)
    Right tabIni -> do
      step "Translate"
      entryPoint <- testDefaultEntryPointIO root' mainFile
      case run $ runReader entryPoint $ runError @JuvixError $ Tree.toAsm tabIni of
        Left err -> assertFailure (prettyString (fromJuvixError @GenericError err))
        Right tab -> do
          let tab' = Asm.fromTree tab
          Asm.asmRunAssertion' tab' expectedFile step
