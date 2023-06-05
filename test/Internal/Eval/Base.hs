module Internal.Eval.Base where

import Base
import Core.Eval.Base
import Data.HashMap.Strict qualified as HashMap
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation (etaExpansionApps)
import Juvix.Compiler.Core.Translation.FromInternal.Data as Core

internalCoreAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
internalCoreAssertion mainFile expectedFile step = do
  step "Translate to Core"
  entryPoint <- defaultEntryPointCwdIO mainFile
  tab0 <- (^. Core.coreResultTable) . snd <$> runIO' entryPoint upToCore
  let tab = etaExpansionApps tab0
  case (tab ^. infoMain) >>= ((tab ^. identContext) HashMap.!?) of
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
                assertEqDiffText ("Check: EVAL output = " <> toFilePath expectedFile) actualOutput expected
        )
    Nothing -> assertFailure ("No main function registered in: " <> toFilePath mainFile)
