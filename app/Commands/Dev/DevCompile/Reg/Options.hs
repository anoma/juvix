{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Reg.Options
  ( module Commands.Dev.DevCompile.Reg.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data RegOptions (k :: InputKind) = RegOptions
  { _regCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (RegOptions k)

makeLenses ''RegOptions

parseReg :: (SingI k) => Parser (RegOptions k)
parseReg = do
  _regCompileCommonOptions <- parseCompileCommonOptions
  pure RegOptions {..}

instance EntryPointOptions (RegOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetReg)
      . applyOptions (opts ^. regCompileCommonOptions)
