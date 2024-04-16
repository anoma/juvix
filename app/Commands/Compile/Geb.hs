module Commands.Compile.Geb where

import Commands.Base
import Commands.Compile.Geb.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend.Geb qualified as Geb
import System.FilePath (takeBaseName)

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => GebOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. gebCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    applyOptions opts
      <$> getEntryPoint (opts' ^. compileInputFile)
  let ext :: FileExt
      ext
        | opts ^. gebOnlyTerm = FileExtJuvixGeb
        | otherwise = FileExtLisp
  gebFile :: Path Abs File <- getOutputFile ext inputFile moutputFile
  let spec
        | opts ^. gebOnlyTerm = Geb.OnlyTerm
        | otherwise =
            Geb.LispPackage
              Geb.LispPackageSpec
                { _lispPackageName = fromString . takeBaseName $ toFilePath gebFile,
                  _lispPackageEntry = "*entry*"
                }
  Geb.Result {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ coreToGeb spec (coreRes ^. coreResultModule)
  writeFileEnsureLn gebFile _resultCode
