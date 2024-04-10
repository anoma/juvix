{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Wasi.Options
  ( module Commands.Compile.Wasi.Options,
    module Commands.Compile.CommonOptions,
    module Commands.Compile.CStage,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Compile.CommonOptions

data WasiOptions (k :: InputKind) = WasiOptions
  { _wasiCompileCommonOptions :: CompileCommonOptions k,
    _wasiCStage :: CStage
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (WasiOptions k)

makeLenses ''WasiOptions

parseWasi :: (SingI k) => Parser (WasiOptions k)
parseWasi = do
  _wasiCompileCommonOptions <- parseCompileCommonOptions
  _wasiCStage <- parseCStage
  pure WasiOptions {..}

wasiOutputFile ::
  (Member App r) =>
  WasiOptions 'InputMain ->
  Sem r (Path Abs File)
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

instance EntryPointOptions (WasiOptions k) where
  applyOptions opts = applyOptions (opts ^. wasiCompileCommonOptions)
