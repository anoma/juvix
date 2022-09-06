module Juvix.Prelude.Trace
  ( module Juvix.Prelude.Trace,
    module Debug.Trace,
  )
where

import Data.Text qualified as Text
import Debug.Trace hiding (trace, traceM, traceShow)
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

trace :: Text -> a -> a
trace = traceLabel ""
{-# WARNING trace "Using trace" #-}

traceM :: Applicative f => Text -> f ()
traceM t = traceLabel "" t (pure ())
{-# WARNING traceM "Using traceM" #-}

traceShow :: Show b => b -> b
traceShow b = traceLabel "" (pack . show $ b) b
{-# WARNING traceShow "Using traceShow" #-}

traceToFile :: FilePath -> Text -> a -> a
traceToFile fpath t a =
  traceLabel (pack ("[" <> fpath <> "]")) t $
    unsafePerformIO $
      do
        writeFile fpath t
        return a
{-# WARNING traceToFile "Using traceToFile" #-}

traceToFile' :: Text -> a -> a
traceToFile' = traceToFile "./juvix.log"
{-# WARNING traceToFile' "Using traceToFile'" #-}

traceToFileM :: (Applicative m) => FilePath -> Text -> a -> m ()
traceToFileM fpath t a = pure (traceToFile fpath t a) $> ()
{-# WARNING traceToFileM "Using traceFileM" #-}
