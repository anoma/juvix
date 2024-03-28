module Commands.CompileNew.Wasi.Options
  ( module Commands.CompileNew.Wasi.Options,
    module Commands.CompileNew.CommonOptions,
    module Commands.CompileNew.CStage,
  )
where

import Commands.Base
import Commands.CompileNew.CStage
import Commands.CompileNew.CommonOptions

data WasiOptions = WasiOptions
  { _wasiCompileCommonOptions :: CompileCommonOptionsMain,
    _wasiCStage :: CStage
  }
  deriving stock (Data)

makeLenses ''WasiOptions

parseWasi :: Parser WasiOptions
parseWasi = do
  _wasiCompileCommonOptions <- parseCompileCommonOptionsMain
  _wasiCStage <- parseCStage
  pure WasiOptions {..}

wasiOutputFile :: (Member App r) => WasiOptions -> Sem r (Path Abs File)
wasiOutputFile opts =
  case opts ^. wasiCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFile (opts ^. wasiCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ case opts ^. wasiCStage of
        CSource -> replaceExtension' cFileExt inputFile
        CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
        CAssembly -> replaceExtension' ".wat" inputFile
        CExecutable -> removeExtension' baseOutputFile
