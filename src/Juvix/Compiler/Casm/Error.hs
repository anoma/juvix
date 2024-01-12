module Juvix.Compiler.Casm.Error where

import Juvix.Compiler.Asm.Language
import Juvix.Data.PPOutput
import Text.Show

data CasmError = CasmError
  { _casmErrorLoc :: Maybe Location,
    _casmErrorMsg :: Text
  }

makeLenses ''CasmError

instance ToGenericError CasmError where
  genericError :: (Member (Reader GenericOptions) r) => CasmError -> Sem r GenericError
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
          msg = pretty (e ^. casmErrorMsg)

instance Pretty CasmError where
  pretty (CasmError {..}) = pretty _casmErrorMsg

instance Show CasmError where
  show (CasmError {..}) = fromText _casmErrorMsg

instance HasLoc CasmError where
  getLoc (CasmError {..}) = fromMaybe defaultLoc _casmErrorLoc
    where
      defaultLoc :: Interval
      defaultLoc = singletonInterval (mkInitialLoc sourcePath)

      sourcePath :: Path Abs File
      sourcePath = $(mkAbsFile "/<casm>")
