{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Vampir.Options
  ( module Commands.Dev.DevCompile.Vampir.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data VampirOptions (k :: InputKind) = VampirOptions
  { _vampirCompileCommonOptions :: CompileCommonOptions k,
    _vampirUnsafe :: Bool
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (VampirOptions k)

makeLenses ''VampirOptions

parseVampir :: (SingI k) => Parser (VampirOptions k)
parseVampir = do
  _vampirCompileCommonOptions <- parseCompileCommonOptions
  _vampirUnsafe <-
    switch
      ( long "unsafe"
          <> help "Disable range and error checking (for targets: vampir)"
      )
  pure VampirOptions {..}

instance EntryPointOptions (VampirOptions k) where
  applyOptions opts =
    set entryPointTarget (Just TargetVampIR)
      . set entryPointUnsafe (opts ^. vampirUnsafe)
      . applyOptions (opts ^. vampirCompileCommonOptions)
