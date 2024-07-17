module Juvix.Data.Logger
  ( defaultLoggerOptions,
    defaultLoggerLevel,
    Logger,
    LoggerOptions (..),
    LogLevel (..),
    logError,
    logProgress,
    logInfo,
    logWarn,
    logDebug,
    runLoggerIO,
    localLogger,
    loggerUseColors,
    loggerLevel,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base
import Juvix.Prelude.Effects.Output
import Juvix.Prelude.Pretty
import Prelude (show)

data LogLevel
  = LogLevelError
  | LogLevelWarn
  | LogLevelInfo
  | LogLevelProgress
  | LogLevelDebug
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show LogLevel where
  show = \case
    LogLevelError -> "error"
    LogLevelWarn -> "warn"
    LogLevelInfo -> "info"
    LogLevelProgress -> "progress"
    LogLevelDebug -> "debug"

instance Pretty LogLevel where
  pretty = pretty . Prelude.show

data Logger :: Effect where
  LogMessage :: LogLevel -> AnsiText -> Logger m ()
  LocalLogger :: LogLevel -> m () -> Logger m ()

data LoggerOptions = LoggerOptions
  { _loggerUseColors :: Bool,
    _loggerLevel :: LogLevel
  }

defaultLoggerLevel :: LogLevel
defaultLoggerLevel = LogLevelProgress

defaultLoggerOptions :: LoggerOptions
defaultLoggerOptions =
  LoggerOptions
    { _loggerUseColors = True,
      _loggerLevel = defaultLoggerLevel
    }

makeSem ''Logger
makeLenses ''LoggerOptions

logError :: (Members '[Logger] r) => AnsiText -> Sem r ()
logError = logMessage LogLevelError

logWarn :: (Members '[Logger] r) => AnsiText -> Sem r ()
logWarn = logMessage LogLevelWarn

logInfo :: (Members '[Logger] r) => AnsiText -> Sem r ()
logInfo = logMessage LogLevelInfo

logProgress :: (Members '[Logger] r) => AnsiText -> Sem r ()
logProgress = logMessage LogLevelProgress

logDebug :: (Members '[Logger] r) => AnsiText -> Sem r ()
logDebug = logMessage LogLevelDebug

runLoggerIO :: forall r a. (Members '[EmbedIO] r) => LoggerOptions -> Sem (Logger ': r) a -> Sem r a
runLoggerIO opts = interp . re
  where
    interp :: Sem (Output AnsiText ': Reader LogLevel ': r) a -> Sem r a
    interp = runReader (opts ^. loggerLevel) . runOutputSem printMsg

    printMsg :: forall r'. (Members '[EmbedIO] r') => AnsiText -> Sem r' ()
    printMsg = hRenderIO (opts ^. loggerUseColors) stderr

re :: Sem (Logger ': r) a -> Sem (Output AnsiText ': Reader LogLevel ': r) a
re = interpretTop2H handler

handler ::
  EffectHandler Logger (Output AnsiText ': Reader LogLevel ': r)
handler localEnv =
  \case
    LocalLogger localLevel localLog ->
      localSeqUnlift localEnv $ \unlift ->
        local (const localLevel) (unlift localLog)
    LogMessage lvl msg -> do
      loggerLvl <- ask
      when (lvl <= loggerLvl) (output msg)
