module Commands.Dev.Nockma.FromAsm where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.FromAsm.Options
import Juvix.Compiler.Asm.Translation.FromSource qualified as Asm
import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Compiler.Nockma.Pretty

runCommand :: forall r. (Members '[Embed IO, App] r) => NockmaFromAsmOptions -> Sem r ()
runCommand opts = do
  afile <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Asm.runParser (toFilePath afile) s of
    Left err -> exitJuvixError (JuvixError err)
    Right tab -> do
      mainSym <- getMain tab
      let (nockFuns, nockMain) = fromAsm mainSym tab
          res = evalCompiledNock nockFuns nockMain
          valStack = getStack ValueStack res
      putStrLn (ppPrint valStack)
  where
    file :: AppPath File
    file = opts ^. nockmaFromAsmOptionsAsmFile

    getMain :: Asm.InfoTable -> Sem r Asm.Symbol
    getMain Asm.InfoTable {..} = case _infoMainFunction of
      Just m -> return m
      Nothing -> do
        putStrLn "Missing `main` function"
        liftIO exitFailure
