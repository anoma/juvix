module Juvix.Compiler.Reg.Error where

import Juvix.Compiler.Reg.Language
import Juvix.Data.PPOutput
import Text.Show

data RegError = RegError
  { _regErrorLoc :: Maybe Location,
    _regErrorMsg :: Text
  }

makeLenses ''RegError

instance ToGenericError RegError where
  genericError :: (Member (Reader GenericOptions) r) => RegError -> Sem r GenericError
  genericError e = ask >>= generr
    where
      generr :: GenericOptions -> Sem r GenericError
      generr _ =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc e
          msg = pretty (e ^. regErrorMsg)

instance Pretty RegError where
  pretty (RegError {..}) = pretty _regErrorMsg

instance Show RegError where
  show (RegError {..}) = fromText _regErrorMsg

instance HasLoc RegError where
  getLoc (RegError {..}) = fromMaybe defaultLoc _regErrorLoc
    where
      defaultLoc :: Interval
      defaultLoc = singletonInterval (mkInitialLoc sourcePath)

      sourcePath :: Path Abs File
      sourcePath = $(mkAbsFile "/<reg>")
