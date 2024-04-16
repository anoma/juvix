{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Geb.Options
  ( module Commands.Compile.Geb.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data GebOptions (k :: InputKind) = GebOptions
  { _gebCompileCommonOptions :: CompileCommonOptions k,
    _gebOnlyTerm :: Bool
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (GebOptions k)

makeLenses ''GebOptions

parseGeb :: (SingI k) => Parser (GebOptions k)
parseGeb = do
  _gebCompileCommonOptions <- parseCompileCommonOptions
  _gebOnlyTerm <-
    switch
      ( short 'G' -- TODO I would like to deprecate the short flag
          <> long "only-term"
          <> help "Produce term output only"
      )
  pure GebOptions {..}

instance EntryPointOptions (GebOptions k) where
  applyOptions opts =
    set entryPointTarget (Just TargetGeb)
      . applyOptions (opts ^. gebCompileCommonOptions)
