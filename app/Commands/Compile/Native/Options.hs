module Commands.Compile.Native.Options
  ( module Commands.Compile.Native.Options,
    module Commands.Compile.CommonOptions,
    module Commands.Compile.CStage,
  )
where

import Commands.Base
import Commands.Compile.CStage
import Commands.Compile.CommonOptions

data NativeOptions = NativeOptions
  { _nativeCompileCommonOptions :: CompileCommonOptionsMain,
    _nativeCStage :: CStage
  }
  deriving stock (Data)

makeLenses ''NativeOptions

parseNative :: Parser NativeOptions
parseNative = do
  _nativeCompileCommonOptions <- parseCompileCommonOptionsJuvixMain
  _nativeCStage <- parseCStage
  pure NativeOptions {..}

nativeOutputFile :: (Member App r) => NativeOptions -> Sem r (Path Abs File)
nativeOutputFile opts =
  case opts ^. nativeCompileCommonOptions . compileOutputFile of
    Just f -> fromAppFile f
    Nothing -> do
      inputFile <- getMainFile (opts ^. nativeCompileCommonOptions . compileInputFile)
      invokeDir <- askInvokeDir
      let baseOutputFile = invokeDir <//> filename inputFile
      return $ case opts ^. nativeCStage of
        CSource -> replaceExtension' cFileExt inputFile
        CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
        CAssembly -> replaceExtension' ".s" inputFile
        CExecutable -> removeExtension' baseOutputFile
