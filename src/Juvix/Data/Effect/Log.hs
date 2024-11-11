module Juvix.Data.Effect.Log where

import Juvix.Prelude.Base

data Log :: Effect where
  Log :: Text -> Log m ()

makeSem ''Log

runLogIO ::
  (Member EmbedIO r) =>
  Sem (Log ': r) a ->
  Sem r a
runLogIO sem = do
  liftIO (hSetBuffering stderr LineBuffering)
  interpret
    ( \case
        Log txt -> hPutStrLn stderr txt
    )
    sem

ignoreLog :: Sem (Log ': r) a -> Sem r a
ignoreLog = interpret $ \case
  Log _ -> return ()
