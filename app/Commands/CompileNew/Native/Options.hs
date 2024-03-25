module Commands.CompileNew.Native.Options where

import CommonOptions

data NativeOptions
  deriving stock (Data)

parseNative :: Parser NativeOptions
parseNative = undefined
