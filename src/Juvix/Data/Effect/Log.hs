module Juvix.Data.Effect.Log where

import Data.Text.IO qualified as Text
import Juvix.Prelude.Base

data Log :: Effect where
  Log :: Text -> Log m ()

makeSem ''Log

runLogIO ::
  (Member EmbedIO r) =>
  Sem (Log ': r) a ->
  Sem r a
runLogIO sem = do
  liftIO (hSetBuffering stdout LineBuffering)
  interpret
    ( \_ -> \case
        Log txt -> liftIO (Text.hPutStrLn stdout txt)
    )
    sem

ignoreLog :: Sem (Log ': r) a -> Sem r a
ignoreLog = interpret $ const $ \case
  Log _ -> return ()
