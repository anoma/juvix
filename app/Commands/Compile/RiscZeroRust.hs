module Commands.Compile.RiscZeroRust where

import Commands.Base
import Commands.Compile.RiscZeroRust.Options
import Commands.Extra.NewCompile
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Compiler.Backend.Rust.Data.Result

runCommand :: forall r. (Members '[App, TaggedLock, EmbedIO] r) => RiscZeroRustOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. riscZeroRustCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputDir = opts ^. riscZeroRustOutputDir
  outDir :: Path Abs Dir <- getOutputDir FileExtRiscZero inputFile moutputDir
  writeDirFiles riscZeroDir outDir
  let outFile :: Path Abs File =
        outDir
          <//> $(mkRelDir "methods")
          <//> $(mkRelDir "guest")
          <//> $(mkRelDir "src")
          <//> $(mkRelFile "main.rs")
  Result {..} <- runPipeline opts inputFile upToRiscZeroRust
  writeFileEnsureLn outFile _resultRustCode
  where
    riscZeroDir :: [(FilePath, ByteString)]
    riscZeroDir = $(FE.makeRelativeToProject "runtime/rust/risc0" >>= FE.embedDir)

    writeDirFiles :: [(FilePath, ByteString)] -> Path Abs Dir -> Sem r ()
    writeDirFiles fs outDir = do
      let fs' = map (first relFile) fs
      forM_ (first (outDir <//>) <$> fs') (uncurry writeFile)
      where
        writeFile :: Path Abs File -> ByteString -> Sem r ()
        writeFile p bs = do
          ensureDir (parent p)
          liftIO $ BS.writeFile (toFilePath p) bs
