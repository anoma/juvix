module Juvix.Data.Effect.Log where

import Data.Text.IO qualified as Text
import Juvix.Prelude.Base

data Log m a where
  Log :: Text -> Log m ()

makeSem ''Log

runLogIO ::
  Member (Embed IO) r =>
  InterpreterFor Log r
runLogIO sem = do
  embed (hSetBuffering stdout LineBuffering)
  interpret
    ( \case
        Log txt -> embed (Text.hPutStrLn stdout txt)
    )
    sem
