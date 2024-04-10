{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Native.Options
  ( module Commands.Compile.Native.Options,
    module Commands.Compile.CommonOptions,
    module Commands.Compile.CStage,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Compile.CommonOptions

data NativeOptions (k :: InputKind) = NativeOptions
  { _nativeCompileCommonOptions :: CompileCommonOptions k,
    _nativeCStage :: CStage
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (NativeOptions k)

makeLenses ''NativeOptions

parseNative :: forall (k :: InputKind). SingI k => Parser (NativeOptions k)
parseNative = do
  _nativeCompileCommonOptions <- parseCompileCommonOptions
  _nativeCStage <- parseCStage
  pure NativeOptions {..}

nativeOutputFile :: forall (k :: InputKind) r. (SingI k, Member App r) => (NativeOptions k) -> Sem r (Path Abs File)
nativeOutputFile opts =
  case opts ^. nativeCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFileFromInputFileType @k (opts ^. nativeCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ case opts ^. nativeCStage of
        CSource -> replaceExtension' cFileExt inputFile
        CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
        CAssembly -> replaceExtension' ".s" inputFile
        CExecutable -> removeExtension' baseOutputFile

instance EntryPointOptions (NativeOptions k) where
  applyOptions opts = applyOptions (opts ^. nativeCompileCommonOptions)
