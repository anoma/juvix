module Commands.Dev.MigrateJuvixYaml.Options where

import CommonOptions

newtype MigrateJuvixYamlOptions = MigrateJuvixYamlOptions
  { _migrateJuvixYamlOptionsForce :: Bool
  }
  deriving stock (Data)

makeLenses ''MigrateJuvixYamlOptions

parseMigrateJuvixYaml :: Parser MigrateJuvixYamlOptions
parseMigrateJuvixYaml = do
  _migrateJuvixYamlOptionsForce <-
    switch
      ( long "force"
          <> short 'f'
          <> help "Overwrite existing Package.juvix"
      )
  pure MigrateJuvixYamlOptions {..}
