module Juvix.Compiler.Reg.Interpreter.Error where

import Control.Exception qualified as Exception
import GHC.Show
import Juvix.Compiler.Reg.Error
import Juvix.Compiler.Reg.Language.Base

data RunError = RunError
  { _runErrorMsg :: Text,
    _runErrorLoc :: Maybe Location
  }

makeLenses ''RunError

instance Show RunError where
  show :: RunError -> String
  show RunError {..} =
    "runtime error: "
      ++ fromText _runErrorMsg

instance Exception.Exception RunError

throwRunError :: Text -> Maybe Location -> a
throwRunError msg loc = Exception.throw (RunError msg loc)

catchRunError :: (MonadIO m) => a -> m (Either RegError a)
catchRunError a =
  liftIO $
    Exception.catch
      (Exception.evaluate a >>= \a' -> return $ Right a')
      (\(ex :: RunError) -> return $ Left (toRegError ex))

toRegError :: RunError -> RegError
toRegError RunError {..} =
  RegError
    { _regErrorMsg = "runtime error: " <> _runErrorMsg,
      _regErrorLoc = _runErrorLoc
    }
