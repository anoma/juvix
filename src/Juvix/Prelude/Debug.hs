module Juvix.Prelude.Debug where

import Debug.Trace
import GHC.IO (unsafePerformIO)
import Juvix.Prelude.Base

setDebugMsg :: String -> String
setDebugMsg msg = "[debug] " <> fmsg
  where
    fmsg
      | null msg = ""
      | otherwise = msg <> " :\n"

debugLText :: String -> Text -> a -> a
debugLText msg a = trace (setDebugMsg msg <> unpack a)
{-# WARNING debugLText "Use debugLText" #-}

debugText :: Text -> a -> a
debugText = debugLText ""
{-# WARNING debugText "Use debugText" #-}

debugThis :: Show b => b -> b
debugThis b = debugLText "" (pack . show $ b) b
{-# WARNING debugThis "Use debugThis" #-}

debugTextToFile :: FilePath -> Text -> a -> a
debugTextToFile fpath t a =
  debugLText ("[" <> fpath <> "]") t $
    unsafePerformIO $
      do
        writeFile fpath t
        return a
{-# WARNING debugTextToFile "Use debugTextToFile" #-}

debugTextToFile' :: Text -> a -> a
debugTextToFile' = debugTextToFile "./juvix.log"
{-# WARNING debugTextToFile' "Use debugTextToFile'" #-}

debugToFileM :: (Applicative m) => FilePath -> Text -> a -> m ()
debugToFileM fpath t a = pure (debugTextToFile fpath t a) $> ()
{-# WARNING debugToFileM "Use debugFileM" #-}
