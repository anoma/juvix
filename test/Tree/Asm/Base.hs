module Tree.Asm.Base where

import Asm.Run.Base qualified as Asm
import Base
import Juvix.Compiler.Asm.Translation.FromTree qualified as Asm
import Juvix.Compiler.Tree.Translation.FromSource
import Juvix.Data.PPOutput

treeAsmAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
treeAsmAssertion mainFile expectedFile step = do
  step "Parse"
  s <- readFile mainFile
  case runParser mainFile s of
    Left err -> assertFailure (show (pretty err))
    Right tabIni -> do
      step "Translate"
      let tab = Asm.fromTree tabIni
      Asm.asmRunAssertion' tab expectedFile step
