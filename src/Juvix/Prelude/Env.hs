module Juvix.Prelude.Env where

import Juvix.Prelude.Base
import Juvix.Prelude.Path
import System.Environment as E

-- | Environment variables relevant to Juvix
data EnvVar
  = EnvWasiSysrootPath
  | EnvAnomaPath
  deriving stock (Show, Eq)

envVarString :: EnvVar -> String
envVarString = \case
  EnvWasiSysrootPath -> "WASI_SYSROOT_PATH"
  EnvAnomaPath -> "ANOMA_PATH"

envVarHint :: EnvVar -> Maybe String
envVarHint = \case
  EnvWasiSysrootPath -> Just "It should point to the location of the wasi-clib sysroot"
  EnvAnomaPath -> Just "It should point to the location of the Anoma repository"

getEnvVar :: (MonadIO m) => EnvVar -> m String
getEnvVar var = fromMaybeM (error (pack msg)) (liftIO (E.lookupEnv (envVarString var)))
  where
    msg :: String
    msg = "Missing environment variable " <> envVarString var <> maybe "" (". " <>) (envVarHint var)

getAnomaPathAbs :: (MonadIO m) => m (Path Abs Dir)
getAnomaPathAbs = absDir <$> getEnvVar EnvAnomaPath

getWasiSysrootPathStr :: (MonadIO m) => m String
getWasiSysrootPathStr = getEnvVar EnvWasiSysrootPath

getWasiSysrootPath :: (MonadIO m) => m (Path Abs Dir)
getWasiSysrootPath = absDir <$> getEnvVar EnvWasiSysrootPath
