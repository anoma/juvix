module Compilation.Base where

import Base
import Core.Eval.Base
import Data.HashMap.Strict qualified as HashMap
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Pipeline

compileAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion mainFile expectedFile step = do
  step "Translate to JuvixCore"
  cwd <- getCurrentDir
  let entryPoint = defaultEntryPoint cwd mainFile
  tab' <- (^. Core.coreResultTable) . snd <$> runIO' iniState entryPoint upToCore
  let tab = Core.toEval tab'
  case (tab ^. Core.infoMain) >>= ((tab ^. Core.identContext) HashMap.!?) of
    Just node -> do
      withTempDir'
        ( \dirPath -> do
            let outputFile = dirPath <//> $(mkRelFile "out.out")
            hout <- openFile (toFilePath outputFile) WriteMode
            step "Evaluate"
            r' <- doEval mainFile hout tab node
            case r' of
              Left err -> do
                hClose hout
                assertFailure (show (pretty err))
              Right value -> do
                unless
                  (Info.member kNoDisplayInfo (getInfo value))
                  (hPutStrLn hout (ppPrint value))
                hClose hout
                actualOutput <- TIO.readFile (toFilePath outputFile)
                step "Compare expected and actual program output"
                expected <- TIO.readFile (toFilePath expectedFile)
                assertEqDiff ("Check: EVAL output = " <> toFilePath expectedFile) actualOutput expected
        )
    Nothing -> assertFailure ("No main function registered in: " <> toFilePath mainFile)
