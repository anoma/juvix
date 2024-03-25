module Commands.CompileNew.Native.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data NativeOptions = NativeOptions
  { _nativeCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseNative :: Parser NativeOptions
parseNative = do
  _nativeCompileCommonOptions <- parseCompileCommonOptions
  pure NativeOptions {..}
