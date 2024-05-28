{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.NativeRust.Options
  ( module Commands.Dev.DevCompile.NativeRust.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import Commands.Extra.Rust
import CommonOptions

data NativeRustOptions (k :: InputKind) = NativeRustOptions
  { _nativeRustCompileCommonOptions :: CompileCommonOptions k,
    _nativeRustStackSize :: Maybe Int
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (NativeRustOptions k)

makeLenses ''NativeRustOptions

parseNativeRust :: (SingI k) => Parser (NativeRustOptions k)
parseNativeRust = do
  _nativeRustCompileCommonOptions <- parseCompileCommonOptions
  _nativeRustStackSize <-
    optional
      ( option
          (fromIntegral <$> naturalNumberOpt)
          ( long "stack-size"
              <> help ("Stack size limit (default: " <> show defaultRustStackSize <> ")")
          )
      )
  pure NativeRustOptions {..}

instance EntryPointOptions (NativeRustOptions k) where
  applyOptions opts =
    set entryPointTarget (Just TargetRust)
      . applyOptions (opts ^. nativeRustCompileCommonOptions)
