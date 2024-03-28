module Commands.CompileNew.Native where

import Commands.Base
import Commands.CompileNew.Native.Options
import Commands.Extra.NewCompile

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => NativeOptions -> Sem r ()
runCommand opts = do
  coreRes <- fromCompileCommonOptionsMain (opts ^. nativeCompileCommonOptions) >>= compileToCore
  undefined
