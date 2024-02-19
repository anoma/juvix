module Commands.Dev.Reg.Read where

import Commands.Base
import Commands.Dev.Reg.Read.Options
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Reg.Transformation qualified as Reg
import Juvix.Compiler.Reg.Translation.FromSource qualified as Reg
import RegInterpreter

runCommand :: forall r. (Members '[EmbedIO, App] r) => RegReadOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  s <- readFile (toFilePath afile)
  case Reg.runParser (toFilePath afile) s of
    Left err ->
      exitJuvixError (JuvixError err)
    Right tab -> do
      r <- runError @JuvixError (Reg.applyTransformations (project opts ^. regReadTransformations) tab)
      case r of
        Left err -> exitJuvixError (JuvixError err)
        Right tab' -> do
          unless (project opts ^. regReadNoPrint) $
            renderStdOut (Reg.ppOutDefault tab' tab')
          doRun tab'
  where
    file :: AppPath File
    file = opts ^. regReadInputFile

    doRun :: Reg.InfoTable -> Sem r ()
    doRun tab'
      | project opts ^. regReadRun = do
          putStrLn "--------------------------------"
          putStrLn "|            Run               |"
          putStrLn "--------------------------------"
          runReg tab'
      | otherwise = return ()
