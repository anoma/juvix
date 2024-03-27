module Commands.CompileNew.Native.Options
  ( module Commands.CompileNew.Native.Options,
    module Commands.CompileNew.CommonOptions,
    module Commands.CompileNew.CStage,
  )
where

import Commands.Base
import Commands.CompileNew.CStage
import Commands.CompileNew.CommonOptions

data NativeOptions = NativeOptions
  { _nativeCompileCommonOptions :: CompileCommonOptions,
    _nativeCStage :: CStage
  }
  deriving stock (Data)

makeLenses ''NativeOptions

parseNative :: Parser NativeOptions
parseNative = do
  _nativeCompileCommonOptions <- parseCompileCommonOptions
  _nativeCStage <- parseCStage
  pure NativeOptions {..}

nativeOutputAppFile :: (Member App r) => NativeOptions -> Sem r (Path Abs File)
nativeOutputAppFile opts = do
  inputFile <- getMainFile (opts ^. nativeCompileCommonOptions . compileInputFile)
  invokeDir <- askInvokeDir
  let baseOutputFile = invokeDir <//> filename inputFile
  return $ case opts ^. nativeCStage of
    CSource -> replaceExtension' cFileExt inputFile
    CPreprocess -> addExtension' cFileExt (addExtension' ".out" (removeExtension' inputFile))
    CAssembly -> replaceExtension' ".s" inputFile
    CExecutable -> removeExtension' baseOutputFile
