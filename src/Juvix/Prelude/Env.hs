module Juvix.Prelude.Env where

import Juvix.Prelude.Base
import Juvix.Prelude.Path
import System.Environment

-- | Environment variables relevant to Juvix
data EnvVar
  = EnvWasiSysrootPath
  deriving stock (Show, Eq)

envVarString :: EnvVar -> String
envVarString = \case
  EnvWasiSysrootPath -> "WASI_SYSROOT_PATH"

envVarHint :: EnvVar -> Maybe String
envVarHint = \case
  EnvWasiSysrootPath -> Just "Set to the location of the wasi-clib sysroot"

getEnvVar :: MonadIO m => EnvVar -> m String
getEnvVar var = fromMaybeM (error (pack msg)) (liftIO (lookupEnv (envVarString var)))
  where
    msg :: String
    msg = "Missing environment variable " <> envVarString var <> maybe "" (". " <>) (envVarHint var)

getWasiSysrootPathStr :: MonadIO m => m String
getWasiSysrootPathStr = getEnvVar EnvWasiSysrootPath

getWasiSysrootPath :: MonadIO m => m (Path Abs Dir)
getWasiSysrootPath = absDir <$> getEnvVar EnvWasiSysrootPath
