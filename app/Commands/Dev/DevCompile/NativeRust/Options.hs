{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.NativeRust.Options
  ( module Commands.Dev.DevCompile.NativeRust.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data NativeRustOptions (k :: InputKind) = NativeRustOptions
  { _nativeRustCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (NativeRustOptions k)

makeLenses ''NativeRustOptions

parseNativeRust :: (SingI k) => Parser (NativeRustOptions k)
parseNativeRust = do
  _nativeRustCompileCommonOptions <- parseCompileCommonOptions
  pure NativeRustOptions {..}

instance EntryPointOptions (NativeRustOptions k) where
  applyOptions opts =
    set entryPointTarget (Just TargetRust)
      . applyOptions (opts ^. nativeRustCompileCommonOptions)
