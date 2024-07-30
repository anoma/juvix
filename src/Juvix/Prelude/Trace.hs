module Juvix.Prelude.Trace
  ( module Juvix.Prelude.Trace,
    module Debug.Trace,
  )
where

import Data.Text qualified as Text
import Debug.Trace hiding (trace, traceM, traceShow, traceWith)
import Debug.Trace qualified as T
import GHC.IO (unsafePerformIO)
import Juvix.Prelude.Base

setDebugMsg :: Text -> Text
setDebugMsg msg = "[debug] " <> fmsg <> "\n"
  where
    fmsg
      | Text.null msg = ""
      | otherwise = msg <> " :"

traceLabel :: Text -> Text -> a -> a
traceLabel msg a = T.trace (unpack $ setDebugMsg msg <> a)
{-# WARNING traceLabel "Using traceLabel" #-}

traceWith :: (a -> Text) -> a -> a
traceWith f a = trace (f a) a
{-# WARNING traceWith "Using traceWith" #-}

trace :: Text -> a -> a
trace = traceLabel ""
{-# WARNING trace "Using trace" #-}

traceM :: (Applicative f) => Text -> f ()
traceM t = traceLabel "" t (pure ())
{-# WARNING traceM "Using traceM" #-}

traceShow :: (Show b) => b -> b
traceShow b = traceLabel "" (pack . show $ b) b
{-# WARNING traceShow "Using traceShow" #-}

traceToFile :: Path Abs File -> Text -> a -> a
traceToFile fpath t a =
  traceLabel (pack ("[" <> toFilePath fpath <> "]")) t $
    unsafePerformIO $
      do
        writeFileEnsureLn fpath t
        return a
{-# WARNING traceToFile "Using traceToFile" #-}

traceToFileM :: (Applicative m) => Path Abs File -> Text -> a -> m ()
traceToFileM fpath t a = pure (traceToFile fpath t a) $> ()
{-# WARNING traceToFileM "Using traceFileM" #-}
